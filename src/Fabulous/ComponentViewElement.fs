namespace Fabulous

type IComponentHandler<'arg, 'msg, 'model, 'externalMsg> =
    abstract CreateRunner: unit -> IRunner<'arg, 'msg, 'model, 'externalMsg>
    abstract GetRunnerForTarget: obj -> IRunner<'arg, 'msg, 'model, 'externalMsg> voption
    abstract SetRunnerForTarget: IRunner<'arg, 'msg, 'model, 'externalMsg> voption * obj -> unit

/// Represent a component with its own internal runner
type IComponentViewElement =
    inherit IViewElement
    /// Create the target control from this ViewElement
    abstract CreateForTarget: ProgramDefinition * obj -> obj
    abstract StartRunner: parent: obj -> unit
    abstract AttachView: view: obj * parent: obj * prevViewElement: IViewElement -> unit
    abstract DetachView: view: obj * parent: obj -> unit
    abstract StopRunner: parent: obj -> unit

type ComponentViewElement<'arg, 'msg, 'model, 'state, 'externalMsg>
    (
        handler: IComponentHandler<'arg, 'msg, 'model, 'externalMsg>,
        runnerDefinition: RunnerDefinition<'arg, 'msg, 'model, 'externalMsg>,
        key: string,
        arg: 'arg,
        state: (('state -> 'msg) * 'state) voption,
        externalMsg: ('externalMsg -> unit) voption
    ) =
    
    let mutable currentView = ValueNone

    let withExternalMsgsIfNeeded (runnerDefinition: RunnerDefinition<'arg, 'msg, 'model, 'externalMsg>) =
        let view model dispatch =
            let v = (runnerDefinition.view model dispatch)
            currentView <- ValueSome v
            v
        
        let runnerDefinition = { runnerDefinition with view = view }
        match externalMsg with
        | ValueNone -> runnerDefinition
        | ValueSome onExternalMsg ->
            let init arg =
                let initModel,cmd,externalMsgs = runnerDefinition.init arg
                externalMsgs |> List.iter onExternalMsg
                initModel,cmd,externalMsgs

            let update msg model =
                let newModel,cmd,externalMsgs = runnerDefinition.update msg model
                externalMsgs |> List.iter onExternalMsg
                newModel,cmd,externalMsgs

            { runnerDefinition with
                init = init
                update = update }

    let dispatchStateChangedIfNeeded (runner: IRunner<'arg, 'msg, 'model, 'externalMsg>) =
        match state with
        | ValueNone -> ()
        | ValueSome (onStateChanged, state) ->
            let msg = onStateChanged state
            runner.Dispatch(msg)

    let getKeyForRunnerFromParent (parent: obj) =
        System.String.Format("{0}_{1}", parent.GetHashCode(), key)
        |> box

    member internal x.CurrentView 
      with get () = currentView
      and set v = currentView <- v

    member x.TargetType = runnerDefinition.GetType()
    
    member x.RunnerDefinition = runnerDefinition
    
    member x.Key = key

    interface IComponentViewElement with
        member x.Create(_, parentOpt) =
            let runnerDefinition = withExternalMsgsIfNeeded runnerDefinition
            let runner = handler.CreateRunner()
            runner.Start(runnerDefinition, arg)
            dispatchStateChangedIfNeeded runner
            
            let target = runner.CreateView(parentOpt)
            handler.SetRunnerForTarget(ValueSome runner, target)
            target

        member x.CreateForTarget(_, target) =
            let runnerDefinition = withExternalMsgsIfNeeded runnerDefinition
            let runner = handler.CreateRunner()
            runner.Start(runnerDefinition, arg)
            dispatchStateChangedIfNeeded runner
            
            handler.SetRunnerForTarget(ValueSome runner, target)
            target

        member x.Update(_, prevOpt, target) =
            match handler.GetRunnerForTarget(target) with
            | ValueNone -> failwith "Can't reuse a control without an associated runner"
            | ValueSome runner ->
                // Only change the definition when it's actually a different runner definition
                match prevOpt with
                | ValueSome (:? ComponentViewElement<'arg, 'msg, 'model, 'state, 'externalMsg> as prev) when
                    prev.Key = x.Key || System.Object.ReferenceEquals(prev.RunnerDefinition, runnerDefinition) ->
                    currentView <- ValueSome runner.LastViewData
                | _ ->
                    let runnerDefinition = withExternalMsgsIfNeeded runnerDefinition
                    runner.Restart(runnerDefinition, arg)
                    
                dispatchStateChangedIfNeeded runner
                
        member x.Unmount(target) =
            match handler.GetRunnerForTarget(target) with
            | ValueNone -> ()
            | ValueSome runner ->
                runner.Stop()
                runner.DetachView()
                handler.SetRunnerForTarget(ValueNone, target)

        member x.StartRunner(parent) =
            let key = getKeyForRunnerFromParent parent
            match handler.GetRunnerForTarget(key) with
            | ValueSome _ -> failwith "Could not start runner because it is already started!"
            | _ ->                
                let runnerDefinition = withExternalMsgsIfNeeded runnerDefinition
                let runner = handler.CreateRunner()
                runner.Start(runnerDefinition, arg)
                dispatchStateChangedIfNeeded runner
                handler.SetRunnerForTarget(ValueSome runner, key)

        member x.StopRunner(parent) =
            let key = getKeyForRunnerFromParent parent
            match handler.GetRunnerForTarget(key) with
            | ValueNone -> failwith "Could not stop runner because it is already stopped!"
            | ValueSome runner ->
                runner.Stop()
                handler.SetRunnerForTarget(ValueNone, key)

        member x.AttachView(view, parent, previousViewElement) =
            let key = getKeyForRunnerFromParent parent
            match handler.GetRunnerForTarget(key) with
            | ValueSome runner ->
                handler.SetRunnerForTarget(ValueNone, key)
                handler.SetRunnerForTarget(ValueSome runner, view)
                runner.AttachView(view, ValueSome previousViewElement)
            | _ -> failwith "Could not attach view because runner is not started!"
                
            
        member x.DetachView(view, parent) =
            match handler.GetRunnerForTarget(view) with
            | ValueSome runner ->
                runner.DetachView()
                handler.SetRunnerForTarget(ValueNone, view)
                handler.SetRunnerForTarget(ValueSome runner, getKeyForRunnerFromParent parent)

            | ValueNone -> failwithf "Could not find runner to detach for view %O" view
        
        /// Get an attribute of the visual element
        member x.TryGetAttributeKeyed(key) = currentView |> ValueOption.bind (fun x -> x.TryGetAttributeKeyed key)

        /// Get an attribute of the visual element
        member x.TryGetAttribute(name) = currentView |> ValueOption.bind (fun x -> x.TryGetAttribute name)

        /// Get an attribute of the visual element
        member x.GetAttributeKeyed(key) = currentView.Value.GetAttributeKeyed key

        /// Remove an attribute from the visual element
        member x.RemoveAttribute(name) =
            match currentView with
            | ValueSome currentView -> currentView.RemoveAttribute(name)
            | ValueNone _ -> false, x :> IViewElement

        /// Remove an attribute from the visual element
        member x.RemoveAttributeKeyed(attrKey) =
            match currentView with
            | ValueSome currentView ->
                let ok, v = currentView.RemoveAttributeKeyed(attrKey)
                if ok then
                    let c = ComponentViewElement(handler, runnerDefinition, key, arg, state, externalMsg)
                    c.CurrentView <- ValueSome v
                    ok, c :> IViewElement
                else
                    false, x :> IViewElement
            | ValueNone _ -> false, x :> IViewElement
        
        member x.TryKey = ValueSome key
        member x.TargetType = x.TargetType