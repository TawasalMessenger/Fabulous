namespace Fabulous.XamarinForms

open Fabulous

module Component =
    let ComponentRunnerProperty = Xamarin.Forms.BindableProperty.CreateAttached("ComponentRunner", typeof<obj>, typeof<Xamarin.Forms.BindableObject>, null)

    let canReuseView (prev: IComponentViewElement) (curr: IComponentViewElement) =
        prev.TargetType = curr.TargetType

    type ComponentHandler<'arg, 'msg, 'model, 'externalMsg>() =
        interface IComponentHandler<'arg, 'msg, 'model, 'externalMsg> with
            member x.CreateRunner() = Runner<'arg, 'msg, 'model, 'externalMsg>() :> IRunner<'arg, 'msg, 'model, 'externalMsg>

            member x.GetRunnerForTarget(target) =
                match (target :?> Xamarin.Forms.BindableObject).GetValue(ComponentRunnerProperty) with
                | null -> ValueNone
                | runner -> ValueSome (runner :?> IRunner<'arg, 'msg, 'model, 'externalMsg>)

            member x.SetRunnerForTarget(runnerOpt, target) =
                let bindableObject = target :?> Xamarin.Forms.BindableObject
                match runnerOpt with
                | ValueNone -> bindableObject.ClearValue(ComponentRunnerProperty)
                | ValueSome runner -> bindableObject.SetValue(ComponentRunnerProperty, runner)

[<AbstractClass; Sealed>]
type Component() =
    static member inline forProgram(runnerType, runnerId, program, ?state: (('state -> 'msg) * 'state), ?externalMsg, ?key) =
        let stateOpt = match state with Some x -> ValueSome x | None -> ValueNone
        let externalMsgOpt = match externalMsg with Some x -> ValueSome x | None -> ValueNone
        let key = match key with Some x -> ValueSome x | None -> ValueNone
        
        let handler = Component.ComponentHandler<unit, 'msg, 'model, 'externalMsg>()
        ComponentViewElement(handler, program, runnerType, runnerId, (), key, stateOpt, externalMsgOpt)
    
    static member inline forProgramWithArgs(runnerType, runnerId, program, args, ?state: (('state -> 'msg) * 'state), ?externalMsg, ?key) =
        let stateOpt = match state with Some x -> ValueSome x | None -> ValueNone
        let externalMsgOpt = match externalMsg with Some x -> ValueSome x | None -> ValueNone
        let key = match key with Some x -> ValueSome x | None -> ValueNone
        
        let handler = Component.ComponentHandler<'arg, 'msg, 'model, 'externalMsg>()
        ComponentViewElement(handler, program, runnerType, runnerId, args, key, stateOpt, externalMsgOpt)