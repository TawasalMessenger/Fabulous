// Copyright Fabulous contributors. See LICENSE.md for license.
namespace Fabulous.XamarinForms

open Fabulous

[<AutoOpen>]
module InputTypes =

    /// Represents a dimension for either the row or column definition of a Grid
    type Dimension =
        /// Use a size that fits the children of the row or column.
        | Auto
        /// Use a proportional size of 1
        | Star
        /// Use a proportional size defined by the associated value
        | Stars of float
        /// Use the associated value as the number of device-specific units.
        | Absolute of float

    /// Defines if the action should be animated or not
    type AnimationKind =
        | Animated
        | NotAnimated

    /// Defines a scroll request to an item
    type ScrollToItem =
        { /// Zero-based index of the item to scroll to
          Index: int
          /// Position to use
          Position: Xamarin.Forms.ScrollToPosition
          /// Determines whether the scroll will be animated or not
          Animate: AnimationKind }

    /// Defines a scroll request to an item in a grouped list
    type ScrollToGroupedItem =
        { /// Zero-based index of the group containing the item to scroll to
          GroupIndex: int
          /// Zero-based index of the item to scroll to
          ItemIndex: int
          /// Position to use
          Position: Xamarin.Forms.ScrollToPosition
          /// Determines whether the scroll will be animated or not
          Animate: AnimationKind }

    /// Represents an image source
    module Image =
        type Value =
            | ImagePath of string
            | ImageBytes of byte[]
            | ImageStream of System.IO.Stream
            | ImageFont of Xamarin.Forms.FontImageSource
            | ImageSource of Xamarin.Forms.ImageSource

        /// A path to the image file (local or network file)
        let fromPath path = ImagePath path
        /// A byte array representing the image
        let fromBytes bytes = ImageBytes bytes
        /// A data stream representing the image
        let fromStream stream = ImageStream stream
        /// A Font image
        let fromFont fontImageSource = ImageFont fontImageSource
        /// An already defined ImageSource
        let fromImageSource imageSource = ImageSource imageSource
        
    /// Represents a font size
    module FontSize =
        type Value =
            | NamedSize of Xamarin.Forms.NamedSize
            | Size of float

        /// Use a named size
        let fromNamedSize namedSize = NamedSize namedSize
        /// Use a value as the size
        let fromValue value = Size value

    /// Represents a content that can be defined with several types
    module Content =
        type Value =
            | String of string
            | ViewElement of IViewElement

        /// A string used as content
        let fromString str = String str
        /// An element used as content
        let fromElement viewElement = ViewElement viewElement

    module Points =
        type Value =
            | String of string
            | PointsList of Xamarin.Forms.Point list

        let fromString str = String str
        let fromList points = PointsList points

    module Figures =
        type Value =
            | String of string
            | FiguresList of IViewElement array

        let fromString str = String str
        let fromList lst = FiguresList (Array.ofList lst)

    module StructuredItems =
        type Value =
            | Text of string
            | ViewElement of IViewElement

        /// A string used as content 
        let fromString str = Text str
        /// An element used as content
        let fromElement viewElement = ViewElement viewElement
        
    /// Represents a text value for Xamarin.Forms.Label
    module LabelText =
        type Value =
            | PlainString of string
            | FormattedString of IViewElement
            
        /// Use a plain string
        let fromString str = PlainString str
        /// Use a formatted string
        let fromFormattedString viewElement = FormattedString viewElement