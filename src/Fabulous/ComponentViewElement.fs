namespace Fabulous

type IComponentHandler<'arg, 'msg, 'model, 'externalMsg> =
    abstract CreateRunner: unit -> IRunner<'arg, 'msg, 'model, 'externalMsg>
    abstract GetRunnerForTarget: obj -> IRunner<'arg, 'msg, 'model, 'externalMsg> voption
    abstract SetRunnerForTarget: IRunner<'arg, 'msg, 'model, 'externalMsg> voption * obj -> unit

/// Represent a component with its own internal runner
type IComponentViewElement =
    inherit IViewElement
    abstract RunnerType: string
    

type ComponentViewElement<'arg, 'msg, 'model, 'state, 'externalMsg>
    (
        handler: IComponentHandler<'arg, 'msg, 'model, 'externalMsg>,
        runnerDefinition: RunnerDefinition<'arg, 'msg, 'model, 'externalMsg>,
        runnerType: string,
        runnerId: string,
        arg: 'arg,
        key: string voption,
        state: (('state -> 'msg) * 'state) voption,
        externalMsg: ('externalMsg -> unit) voption
    ) =

    let withExternalMsgsIfNeeded (runnerDefinition: RunnerDefinition<'arg, 'msg, 'model, 'externalMsg>) =
        let runnerDefinition = { runnerDefinition with view = runnerDefinition.view }
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
        | ValueSome (onStateChanged, state) when runner.LastState <> box state ->
            let msg = onStateChanged state
            runner.Dispatch(msg)
        | _ -> ()

    member internal x.CurrentView 
      with get () =
          handler.GetRunnerForTarget runnerId
          |> ValueOption.map (fun x -> x.ForceViewData())

    member x.TargetType = runnerDefinition.GetType()
    
    member x.RunnerDefinition = runnerDefinition
    
    member x.Key = key
    
    member x.RunnerId = runnerId

    interface IComponentViewElement with
        member x.Create(_, parentOpt) =
            let runnerDefinition = withExternalMsgsIfNeeded runnerDefinition
            let runner =
                match handler.GetRunnerForTarget runnerId with
                | ValueSome runner ->
                    runner
                | _ ->
                    let runner = handler.CreateRunner()
                    runner.Start(runnerDefinition, arg)
                    handler.SetRunnerForTarget(ValueSome runner, runnerId)
                    runner

            dispatchStateChangedIfNeeded runner            
            let target = runner.CreateView(parentOpt)
            target

        member x.Start() =
            match handler.GetRunnerForTarget(runnerId) with
            | ValueSome _ ->
                ()
                // runner.Start(runnerDefinition, arg)
            | _ ->
                let runnerDefinition = withExternalMsgsIfNeeded runnerDefinition
                let runner = handler.CreateRunner()
                runner.Start(runnerDefinition, arg)
                handler.SetRunnerForTarget(ValueSome runner, runnerId)
                dispatchStateChangedIfNeeded runner
                runner.LastViewData.Start()

        member x.Stop() =
            match handler.GetRunnerForTarget(runnerId) with
            | ValueSome runner ->
                runner.DetachView(true)
                runner.Stop()
                handler.SetRunnerForTarget(ValueNone, runnerId)
            | _ -> ()

        member x.Update(_, prevOpt, target) =
            match handler.GetRunnerForTarget(runnerId) with
            | ValueNone -> failwithf "Can't reuse a control without an started runner (runnerId = %s)" runnerId
            | ValueSome runner ->
                // Only change the definition when it's actually a different runner definition
                match prevOpt with
                | ValueSome (:? ComponentViewElement<'arg, 'msg, 'model, 'state, 'externalMsg> as prev) when
                    (prev :> IComponentViewElement).RunnerType = runnerType ->

                    if prev.RunnerId <> runnerId then
                        match handler.GetRunnerForTarget(prev.RunnerId) with
                        | ValueSome prevRunner ->
                            prevRunner.DetachView(false)
                            runner.AttachView(target, ValueSome prevRunner.LastViewData)
                        | _ ->
                            ()

                | _ ->
                    let runnerDefinition = withExternalMsgsIfNeeded runnerDefinition
                    runner.Restart(runnerDefinition, arg)

                dispatchStateChangedIfNeeded runner
                
        member x.Unmount(target, stopRunner) =
            match handler.GetRunnerForTarget(runnerId) with
            | ValueNone -> ()
            | ValueSome runner ->
                if stopRunner then
                  handler.SetRunnerForTarget(ValueNone, runnerId)
                  runner.Stop()
                runner.DetachView(stopRunner)

        /// Get an attribute of the visual element
        member x.TryGetAttributeKeyed(key) = x.CurrentView |> ValueOption.bind (fun x -> x.TryGetAttributeKeyed key)

        /// Get an attribute of the visual element
        member x.TryGetAttribute(name) = x.CurrentView |> ValueOption.bind (fun x -> x.TryGetAttribute name)

        /// Get an attribute of the visual element
        member x.GetAttributeKeyed(key) = x.CurrentView.Value.GetAttributeKeyed key

        /// Remove an attribute from the visual element
        member x.RemoveAttribute(name) =
            match x.CurrentView with
            | ValueSome currentView -> currentView.RemoveAttribute(name)
            | ValueNone _ -> false, x :> IViewElement

        /// Remove an attribute from the visual element
        member x.RemoveAttributeKeyed(attrKey) =
            match x.CurrentView with
            | ValueSome currentView ->
                let ok, v = currentView.RemoveAttributeKeyed(attrKey)
                if ok then
                    let c = ComponentViewElement(handler, runnerDefinition, runnerType, runnerId, arg, key, state, externalMsg)
                    match handler.GetRunnerForTarget(runnerId) with
                    | ValueSome runner -> runner.LastViewData <- v
                    | _ -> ()
                    ok, c :> IViewElement
                else
                    false, x :> IViewElement
            | ValueNone _ -> false, x :> IViewElement
        
        member x.RunnerType = runnerType
        member x.TryKey = key
        member x.TargetType = x.TargetType