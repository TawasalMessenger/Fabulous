namespace Fabulous

type IComponentHandler<'arg, 'msg, 'model, 'externalMsg> =
    abstract CreateRunner: unit -> IRunner<'arg, 'msg, 'model, 'externalMsg>
    abstract GetRunnerForTarget: obj -> IRunner<'arg, 'msg, 'model, 'externalMsg> voption
    abstract SetRunnerForTarget: IRunner<'arg, 'msg, 'model, 'externalMsg> voption * obj -> unit

/// Represent a component with its own internal runner
type IComponentViewElement =
    inherit IViewElement
    
    abstract CreateForTarget: ProgramDefinition * obj * obj voption -> obj

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

    member x.TargetType = runnerDefinition.GetType()
    
    member x.RunnerDefinition = runnerDefinition
    
    member x.Key = key

    interface IComponentViewElement with
        member x.Create(_, parentOpt) =
            let runnerDefinition = withExternalMsgsIfNeeded runnerDefinition
            let runner = handler.CreateRunner()
            let target = runner.Start(runnerDefinition, ValueNone, parentOpt, arg)
            dispatchStateChangedIfNeeded runner
            handler.SetRunnerForTarget(ValueSome runner, target)
            target
            
        member x.CreateForTarget(_, target, parentOpt) =
            let runnerDefinition = withExternalMsgsIfNeeded runnerDefinition
            let runner = handler.CreateRunner()
            let target = runner.Start(runnerDefinition, ValueSome (box target), parentOpt, arg)
            dispatchStateChangedIfNeeded runner
            handler.SetRunnerForTarget(ValueSome runner, target)
            target
            

        member x.Update(_, prevOpt, target) =
            match handler.GetRunnerForTarget(target) with
            | ValueNone -> failwith "Can't reuse a control without an associated runner"
            | ValueSome runner ->
                // Only change the definition when it's actually a different runner definition
                match prevOpt with
                | ValueSome (:? ComponentViewElement<'arg, 'msg, 'model, 'state, 'externalMsg> as prev) when prev.Key = x.Key && System.Object.ReferenceEquals(prev.RunnerDefinition, runnerDefinition) -> ()
                | _ ->
                    let runnerDefinition = withExternalMsgsIfNeeded runnerDefinition
                    runner.Restart(runnerDefinition, target, arg)
                    
                dispatchStateChangedIfNeeded runner
                
        member x.Unmount(target) =
            match handler.GetRunnerForTarget(target) with
            | ValueNone -> ()
            | ValueSome runner ->
                runner.Stop()
                handler.SetRunnerForTarget(ValueNone, target)
                
        
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
        member x.RemoveAttributeKeyed(key) =
            match currentView with
            | ValueSome currentView -> currentView.RemoveAttributeKeyed(key)
            | ValueNone _ -> false, x :> IViewElement
        
        member x.TryKey = ValueSome key
        member x.TargetType = x.TargetType