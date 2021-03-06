// Copyright Fabulous contributors. See LICENSE.md for license.
namespace Fabulous

open System
open System.Collections.Generic
open Fabulous.Tracing

[<AutoOpen>]
module internal AttributeKeys =
    let attribKeys = Dictionary<string,int>()
    let attribNames = Dictionary<int,string>()


/// Represent an attribute key.
/// Instead of referring to a property/event of a control by its name (string), we refer to it by a key (int)
/// This reduces the memory footprint
[<Struct>]
type AttributeKey<'T> internal (keyv: int) =

    static let getAttribKeyValue (attribName: string) : int =
        match attribKeys.TryGetValue(attribName) with
        | true, keyv -> keyv
        | false, _ ->
            let keyv = attribKeys.Count + 1
            attribKeys.[attribName] <- keyv
            attribNames.[keyv] <- attribName
            keyv

    new (keyName: string) = AttributeKey<'T>(getAttribKeyValue keyName)

    member __.KeyValue = keyv

    member __.Name = AttributeKey<'T>.GetName(keyv)

    static member GetName(keyv: int) =
        match attribNames.TryGetValue(keyv) with
        | true, keyv -> keyv
        | false, _ -> failwithf "unregistered attribute key %d" keyv


type ProgramDefinition =
    { canReuseView: IViewElement -> IViewElement -> bool
      dispatch: obj -> unit
      trace: TraceLevel -> string -> unit
      traceLevel: TraceLevel }

/// Represents a visual element
and IViewElement =
    /// Create the target control from this ViewElement
    abstract Create: ProgramDefinition * obj voption -> obj
    /// Update an existing control by applying the diff between a previous ViewElement and this one
    abstract Update: ProgramDefinition * IViewElement voption * obj -> unit
    /// Signal the ViewElement that its associated control is no longer available
    abstract Unmount: obj -> unit
    /// Try get the key of this ViewElement
    abstract TryKey: string voption with get
    /// Target type of the control that will be created by this ViewElement
    abstract TargetType: Type with get

    /// Get an attribute of the visual element
    abstract TryGetAttributeKeyed: key: AttributeKey<'T> -> 'T voption

    /// Get an attribute of the visual element
    abstract TryGetAttribute: name: string -> 'T voption

    /// Get an attribute of the visual element
    abstract GetAttributeKeyed: key: AttributeKey<'T> -> 'T
    
    abstract RemoveAttribute: name: string -> (bool * IViewElement)
    
    abstract RemoveAttributeKeyed: name: AttributeKey<'T> -> (bool * IViewElement)


type RunnerDefinition<'arg, 'msg, 'model, 'externalMsg> =
    { init: 'arg -> 'model * Cmd<'msg> * ('externalMsg list)
      update: 'msg -> 'model -> 'model * Cmd<'msg> * ('externalMsg list)
      view: 'model -> Dispatch<'msg> -> IViewElement
      subscribe: 'model -> Dispatch<'msg> -> IDisposable
      canReuseView: IViewElement -> IViewElement -> bool
      syncDispatch: Dispatch<'msg> -> Dispatch<'msg>
      syncAction: (unit -> unit) -> (unit -> unit)
      traceLevel: TraceLevel
      trace: TraceLevel -> string -> unit
      onError: string -> Exception -> unit }

/// Responsible for running the MVU loop
type IRunner<'arg, 'msg, 'model, 'externalMsg> =
    /// Start the runner using the given definition
    abstract Start: RunnerDefinition<'arg, 'msg, 'model, 'externalMsg> * obj voption * obj voption * 'arg -> obj
    /// Stop the runner and dispose its subscriptions
    abstract Stop: unit -> unit
    abstract Restart: RunnerDefinition<'arg, 'msg, 'model, 'externalMsg> * obj * 'arg -> unit
    /// Dispatch a message to the MVU loop of this runner
    abstract Dispatch: 'msg -> unit

module internal ProgramTracing =
    let inline traceDebug (definition: ProgramDefinition) = traceDebug definition.trace definition.traceLevel
    let inline traceInfo (definition: ProgramDefinition) = traceInfo definition.trace definition.traceLevel
    let inline traceError (definition: ProgramDefinition) = traceError definition.trace definition.traceLevel

module internal RunnerTracing =
    let inline traceDebug (definition: RunnerDefinition<'arg, 'msg, 'model, 'externalMsg>) = traceDebug definition.trace definition.traceLevel
    let inline traceInfo (definition: RunnerDefinition<'arg, 'msg, 'model, 'externalMsg>) = traceInfo definition.trace definition.traceLevel
    let inline traceError (definition: RunnerDefinition<'arg, 'msg, 'model, 'externalMsg>) = traceError definition.trace definition.traceLevel