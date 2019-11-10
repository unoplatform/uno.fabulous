// Copyright 2018-2019 Fabulous contributors. See LICENSE.md for license.
namespace Fabulous.Uno

#nowarn "59" // cast always holds
#nowarn "66" // cast always holds
#nowarn "67" // cast always holds

open Fabulous

module ViewAttributes =

type ViewBuilders() =
    /// Builds the attributes for a UIElement in the view
    static member inline BuildUIElement(attribCount: int) = 
        let attribBuilder = new AttributesBuilder(attribCount)
        attribBuilder

    static member UpdateUIElement (prevOpt: ViewElement voption, curr: ViewElement, target: Windows.UI.Xaml.UIElement) = 
        ()

    /// Builds the attributes for a FrameworkElement in the view
    static member inline BuildFrameworkElement(attribCount: int) = 
        let attribBuilder = ViewBuilders.BuildUIElement(attribCount)
        attribBuilder

    static member UpdateFrameworkElement (prevOpt: ViewElement voption, curr: ViewElement, target: Windows.UI.Xaml.FrameworkElement) = 
        ViewBuilders.UpdateUIElement (prevOpt, curr, target)

/// Viewer that allows to read the properties of a ViewElement representing a UIElement
type UIElementViewer(element: ViewElement) =
    do if not ((typeof<Windows.UI.Xaml.UIElement>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'Windows.UI.Xaml.UIElement' is expected, but '%s' was provided." element.TargetType.FullName

/// Viewer that allows to read the properties of a ViewElement representing a FrameworkElement
type FrameworkElementViewer(element: ViewElement) =
    inherit UIElementViewer(element)
    do if not ((typeof<Windows.UI.Xaml.FrameworkElement>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'Windows.UI.Xaml.FrameworkElement' is expected, but '%s' was provided." element.TargetType.FullName

[<AbstractClass; Sealed>]
type View private () =

[<AutoOpen>]
module ViewElementExtensions = 

    type ViewElement with

        member inline x.With() =
            x

