// Copyright 2018-2019 Fabulous contributors. See LICENSE.md for license.
namespace Fabulous.Uno

#nowarn "59" // cast always holds
#nowarn "66" // cast always holds
#nowarn "67" // cast always holds

open Fabulous

module ViewAttributes =
    let OpacityAttribKey : AttributeKey<_> = AttributeKey<_>("Opacity")
    let LoadedEventAttribKey : AttributeKey<_> = AttributeKey<_>("LoadedEvent")
    let WidthAttribKey : AttributeKey<_> = AttributeKey<_>("Width")
    let HeightAttribKey : AttributeKey<_> = AttributeKey<_>("Height")
    let RowSpacingAttribKey : AttributeKey<_> = AttributeKey<_>("RowSpacing")
    let OrientationAttribKey : AttributeKey<_> = AttributeKey<_>("Orientation")
    let TextAttribKey : AttributeKey<_> = AttributeKey<_>("Text")
    let ForegroundAttribKey : AttributeKey<_> = AttributeKey<_>("Foreground")

type ViewBuilders() =
    /// Builds the attributes for a DependencyObject in the view
    static member inline BuildDependencyObject(attribCount: int) = 
        let attribBuilder = new AttributesBuilder(attribCount)
        attribBuilder

    static member UpdateDependencyObject (prevOpt: ViewElement voption, curr: ViewElement, target: Windows.UI.Xaml.DependencyObject) = 
        ()

    /// Builds the attributes for a UIElement in the view
    static member inline BuildUIElement(attribCount: int,
                                        ?opacity: float) = 

        let attribCount = match opacity with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = new AttributesBuilder(attribCount)
        match opacity with None -> () | Some v -> attribBuilder.Add(ViewAttributes.OpacityAttribKey, (v)) 
        attribBuilder

    static member UpdateUIElement (prevOpt: ViewElement voption, curr: ViewElement, target: Windows.UI.Xaml.UIElement) = 
        let mutable prevOpacityOpt = ValueNone
        let mutable currOpacityOpt = ValueNone
        for kvp in curr.AttributesKeyed do
            if kvp.Key = ViewAttributes.OpacityAttribKey.KeyValue then 
                currOpacityOpt <- ValueSome (kvp.Value :?> float)
        match prevOpt with
        | ValueNone -> ()
        | ValueSome prev ->
            for kvp in prev.AttributesKeyed do
                if kvp.Key = ViewAttributes.OpacityAttribKey.KeyValue then 
                    prevOpacityOpt <- ValueSome (kvp.Value :?> float)
        // Update properties
        match prevOpacityOpt, currOpacityOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.Opacity <-  currValue
        | ValueSome _, ValueNone -> target.ClearValue Windows.UI.Xaml.UIElement.OpacityProperty
        | ValueNone, ValueNone -> ()

    /// Builds the attributes for a FrameworkElement in the view
    static member inline BuildFrameworkElement(attribCount: int,
                                               ?width: float,
                                               ?height: float,
                                               ?opacity: float,
                                               ?loadedEvent: obj -> unit) = 

        let attribCount = match width with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match height with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match loadedEvent with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = ViewBuilders.BuildUIElement(attribCount, ?opacity=opacity)
        match width with None -> () | Some v -> attribBuilder.Add(ViewAttributes.WidthAttribKey, (v)) 
        match height with None -> () | Some v -> attribBuilder.Add(ViewAttributes.HeightAttribKey, (v)) 
        match loadedEvent with None -> () | Some v -> attribBuilder.Add(ViewAttributes.LoadedEventAttribKey, (fun f -> System.EventHandler<unit>(fun _sender _args -> f _args))(v)) 
        attribBuilder

    static member CreateFrameworkElement () : Windows.UI.Xaml.FrameworkElement =
        new Windows.UI.Xaml.FrameworkElement()

    static member UpdateFrameworkElement (prevOpt: ViewElement voption, curr: ViewElement, target: Windows.UI.Xaml.FrameworkElement) = 
        let mutable prevWidthOpt = ValueNone
        let mutable currWidthOpt = ValueNone
        let mutable prevHeightOpt = ValueNone
        let mutable currHeightOpt = ValueNone
        for kvp in curr.AttributesKeyed do
            if kvp.Key = ViewAttributes.WidthAttribKey.KeyValue then 
                currWidthOpt <- ValueSome (kvp.Value :?> float)
            if kvp.Key = ViewAttributes.HeightAttribKey.KeyValue then 
                currHeightOpt <- ValueSome (kvp.Value :?> float)
        match prevOpt with
        | ValueNone -> ()
        | ValueSome prev ->
            for kvp in prev.AttributesKeyed do
                if kvp.Key = ViewAttributes.WidthAttribKey.KeyValue then 
                    prevWidthOpt <- ValueSome (kvp.Value :?> float)
                if kvp.Key = ViewAttributes.HeightAttribKey.KeyValue then 
                    prevHeightOpt <- ValueSome (kvp.Value :?> float)
        // Update inherited members
        ViewBuilders.UpdateUIElement (prevOpt, curr, target)
        // Update properties
        match prevWidthOpt, currWidthOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.Width <-  currValue
        | ValueSome _, ValueNone -> target.ClearValue Windows.UI.Xaml.FrameworkElement.WidthProperty
        | ValueNone, ValueNone -> ()
        match prevHeightOpt, currHeightOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.Height <-  currValue
        | ValueSome _, ValueNone -> target.ClearValue Windows.UI.Xaml.FrameworkElement.HeightProperty
        | ValueNone, ValueNone -> ()

    static member inline ConstructFrameworkElement(?width: float,
                                                   ?height: float,
                                                   ?opacity: float,
                                                   ?loadedEvent: obj -> unit) = 

        let attribBuilder = ViewBuilders.BuildFrameworkElement(0,
                               ?width=width,
                               ?height=height,
                               ?opacity=opacity,
                               ?loadedEvent=loadedEvent)

        ViewElement.Create<Windows.UI.Xaml.FrameworkElement>(ViewBuilders.CreateFrameworkElement, (fun prevOpt curr target -> ViewBuilders.UpdateFrameworkElement(prevOpt, curr, target)), attribBuilder)

    /// Builds the attributes for a Panel in the view
    static member inline BuildPanel(attribCount: int,
                                    ?width: float,
                                    ?height: float,
                                    ?opacity: float,
                                    ?loadedEvent: obj -> unit) = 
        let attribBuilder = ViewBuilders.BuildFrameworkElement(attribCount, ?width=width, ?height=height, ?opacity=opacity, ?loadedEvent=loadedEvent)
        attribBuilder

    static member CreatePanel () : Windows.UI.Xaml.Controls.Panel =
        new Windows.UI.Xaml.Controls.Panel()

    static member UpdatePanel (prevOpt: ViewElement voption, curr: ViewElement, target: Windows.UI.Xaml.Controls.Panel) = 
        ViewBuilders.UpdateFrameworkElement (prevOpt, curr, target)

    static member inline ConstructPanel(?width: float,
                                        ?height: float,
                                        ?opacity: float,
                                        ?loadedEvent: obj -> unit) = 

        let attribBuilder = ViewBuilders.BuildPanel(0,
                               ?width=width,
                               ?height=height,
                               ?opacity=opacity,
                               ?loadedEvent=loadedEvent)

        ViewElement.Create<Windows.UI.Xaml.Controls.Panel>(ViewBuilders.CreatePanel, (fun prevOpt curr target -> ViewBuilders.UpdatePanel(prevOpt, curr, target)), attribBuilder)

    /// Builds the attributes for a Grid in the view
    static member inline BuildGrid(attribCount: int,
                                   ?rowSpacing: float,
                                   ?width: float,
                                   ?height: float,
                                   ?opacity: float,
                                   ?loadedEvent: obj -> unit) = 

        let attribCount = match rowSpacing with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = ViewBuilders.BuildPanel(attribCount, ?width=width, ?height=height, ?opacity=opacity, ?loadedEvent=loadedEvent)
        match rowSpacing with None -> () | Some v -> attribBuilder.Add(ViewAttributes.RowSpacingAttribKey, (v)) 
        attribBuilder

    static member CreateGrid () : Windows.UI.Xaml.Controls.Grid =
        new Windows.UI.Xaml.Controls.Grid()

    static member UpdateGrid (prevOpt: ViewElement voption, curr: ViewElement, target: Windows.UI.Xaml.Controls.Grid) = 
        let mutable prevRowSpacingOpt = ValueNone
        let mutable currRowSpacingOpt = ValueNone
        for kvp in curr.AttributesKeyed do
            if kvp.Key = ViewAttributes.RowSpacingAttribKey.KeyValue then 
                currRowSpacingOpt <- ValueSome (kvp.Value :?> float)
        match prevOpt with
        | ValueNone -> ()
        | ValueSome prev ->
            for kvp in prev.AttributesKeyed do
                if kvp.Key = ViewAttributes.RowSpacingAttribKey.KeyValue then 
                    prevRowSpacingOpt <- ValueSome (kvp.Value :?> float)
        // Update inherited members
        ViewBuilders.UpdatePanel (prevOpt, curr, target)
        // Update properties
        match prevRowSpacingOpt, currRowSpacingOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.RowSpacing <-  currValue
        | ValueSome _, ValueNone -> target.ClearValue Windows.UI.Xaml.Controls.Grid.RowSpacingProperty
        | ValueNone, ValueNone -> ()

    static member inline ConstructGrid(?rowSpacing: float,
                                       ?width: float,
                                       ?height: float,
                                       ?opacity: float,
                                       ?loadedEvent: obj -> unit) = 

        let attribBuilder = ViewBuilders.BuildGrid(0,
                               ?rowSpacing=rowSpacing,
                               ?width=width,
                               ?height=height,
                               ?opacity=opacity,
                               ?loadedEvent=loadedEvent)

        ViewElement.Create<Windows.UI.Xaml.Controls.Grid>(ViewBuilders.CreateGrid, (fun prevOpt curr target -> ViewBuilders.UpdateGrid(prevOpt, curr, target)), attribBuilder)

    /// Builds the attributes for a StackPanel in the view
    static member inline BuildStackPanel(attribCount: int,
                                         ?orientation: Windows.UI.Xaml.Controls.Orientation,
                                         ?width: float,
                                         ?height: float,
                                         ?opacity: float,
                                         ?loadedEvent: obj -> unit) = 

        let attribCount = match orientation with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = ViewBuilders.BuildPanel(attribCount, ?width=width, ?height=height, ?opacity=opacity, ?loadedEvent=loadedEvent)
        match orientation with None -> () | Some v -> attribBuilder.Add(ViewAttributes.OrientationAttribKey, (v)) 
        attribBuilder

    static member CreateStackPanel () : Windows.UI.Xaml.Controls.StackPanel =
        new Windows.UI.Xaml.Controls.StackPanel()

    static member UpdateStackPanel (prevOpt: ViewElement voption, curr: ViewElement, target: Windows.UI.Xaml.Controls.StackPanel) = 
        let mutable prevOrientationOpt = ValueNone
        let mutable currOrientationOpt = ValueNone
        for kvp in curr.AttributesKeyed do
            if kvp.Key = ViewAttributes.OrientationAttribKey.KeyValue then 
                currOrientationOpt <- ValueSome (kvp.Value :?> Windows.UI.Xaml.Controls.Orientation)
        match prevOpt with
        | ValueNone -> ()
        | ValueSome prev ->
            for kvp in prev.AttributesKeyed do
                if kvp.Key = ViewAttributes.OrientationAttribKey.KeyValue then 
                    prevOrientationOpt <- ValueSome (kvp.Value :?> Windows.UI.Xaml.Controls.Orientation)
        // Update inherited members
        ViewBuilders.UpdatePanel (prevOpt, curr, target)
        // Update properties
        match prevOrientationOpt, currOrientationOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.Orientation <-  currValue
        | ValueSome _, ValueNone -> target.ClearValue Windows.UI.Xaml.Controls.StackPanel.OrientationProperty
        | ValueNone, ValueNone -> ()

    static member inline ConstructStackPanel(?orientation: Windows.UI.Xaml.Controls.Orientation,
                                             ?width: float,
                                             ?height: float,
                                             ?opacity: float,
                                             ?loadedEvent: obj -> unit) = 

        let attribBuilder = ViewBuilders.BuildStackPanel(0,
                               ?orientation=orientation,
                               ?width=width,
                               ?height=height,
                               ?opacity=opacity,
                               ?loadedEvent=loadedEvent)

        ViewElement.Create<Windows.UI.Xaml.Controls.StackPanel>(ViewBuilders.CreateStackPanel, (fun prevOpt curr target -> ViewBuilders.UpdateStackPanel(prevOpt, curr, target)), attribBuilder)

    /// Builds the attributes for a TextBlock in the view
    static member inline BuildTextBlock(attribCount: int,
                                        ?text: string,
                                        ?foreground: Windows.UI.Xaml.Media.Brush,
                                        ?width: float,
                                        ?height: float,
                                        ?opacity: float,
                                        ?loadedEvent: obj -> unit) = 

        let attribCount = match text with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match foreground with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = ViewBuilders.BuildFrameworkElement(attribCount, ?width=width, ?height=height, ?opacity=opacity, ?loadedEvent=loadedEvent)
        match text with None -> () | Some v -> attribBuilder.Add(ViewAttributes.TextAttribKey, (v)) 
        match foreground with None -> () | Some v -> attribBuilder.Add(ViewAttributes.ForegroundAttribKey, (v)) 
        attribBuilder

    static member CreateTextBlock () : Windows.UI.Xaml.Controls.TextBlock =
        new Windows.UI.Xaml.Controls.TextBlock()

    static member UpdateTextBlock (prevOpt: ViewElement voption, curr: ViewElement, target: Windows.UI.Xaml.Controls.TextBlock) = 
        let mutable prevTextOpt = ValueNone
        let mutable currTextOpt = ValueNone
        let mutable prevForegroundOpt = ValueNone
        let mutable currForegroundOpt = ValueNone
        for kvp in curr.AttributesKeyed do
            if kvp.Key = ViewAttributes.TextAttribKey.KeyValue then 
                currTextOpt <- ValueSome (kvp.Value :?> string)
            if kvp.Key = ViewAttributes.ForegroundAttribKey.KeyValue then 
                currForegroundOpt <- ValueSome (kvp.Value :?> Windows.UI.Xaml.Media.Brush)
        match prevOpt with
        | ValueNone -> ()
        | ValueSome prev ->
            for kvp in prev.AttributesKeyed do
                if kvp.Key = ViewAttributes.TextAttribKey.KeyValue then 
                    prevTextOpt <- ValueSome (kvp.Value :?> string)
                if kvp.Key = ViewAttributes.ForegroundAttribKey.KeyValue then 
                    prevForegroundOpt <- ValueSome (kvp.Value :?> Windows.UI.Xaml.Media.Brush)
        // Update inherited members
        ViewBuilders.UpdateFrameworkElement (prevOpt, curr, target)
        // Update properties
        match prevTextOpt, currTextOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.Text <-  currValue
        | ValueSome _, ValueNone -> target.ClearValue Windows.UI.Xaml.Controls.TextBlock.TextProperty
        | ValueNone, ValueNone -> ()
        match prevForegroundOpt, currForegroundOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.Foreground <-  currValue
        | ValueSome _, ValueNone -> target.ClearValue Windows.UI.Xaml.Controls.TextBlock.ForegroundProperty
        | ValueNone, ValueNone -> ()

    static member inline ConstructTextBlock(?text: string,
                                            ?foreground: Windows.UI.Xaml.Media.Brush,
                                            ?width: float,
                                            ?height: float,
                                            ?opacity: float,
                                            ?loadedEvent: obj -> unit) = 

        let attribBuilder = ViewBuilders.BuildTextBlock(0,
                               ?text=text,
                               ?foreground=foreground,
                               ?width=width,
                               ?height=height,
                               ?opacity=opacity,
                               ?loadedEvent=loadedEvent)

        ViewElement.Create<Windows.UI.Xaml.Controls.TextBlock>(ViewBuilders.CreateTextBlock, (fun prevOpt curr target -> ViewBuilders.UpdateTextBlock(prevOpt, curr, target)), attribBuilder)

/// Viewer that allows to read the properties of a ViewElement representing a DependencyObject
type DependencyObjectViewer(element: ViewElement) =
    do if not ((typeof<Windows.UI.Xaml.DependencyObject>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'Windows.UI.Xaml.DependencyObject' is expected, but '%s' was provided." element.TargetType.FullName

/// Viewer that allows to read the properties of a ViewElement representing a UIElement
type UIElementViewer(element: ViewElement) =
    do if not ((typeof<Windows.UI.Xaml.UIElement>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'Windows.UI.Xaml.UIElement' is expected, but '%s' was provided." element.TargetType.FullName
    /// Get the value of the Opacity member
    member this.Opacity = element.GetAttributeKeyed(ViewAttributes.OpacityAttribKey)

/// Viewer that allows to read the properties of a ViewElement representing a FrameworkElement
type FrameworkElementViewer(element: ViewElement) =
    inherit UIElementViewer(element)
    do if not ((typeof<Windows.UI.Xaml.FrameworkElement>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'Windows.UI.Xaml.FrameworkElement' is expected, but '%s' was provided." element.TargetType.FullName
    /// Get the value of the Width member
    member this.Width = element.GetAttributeKeyed(ViewAttributes.WidthAttribKey)
    /// Get the value of the Height member
    member this.Height = element.GetAttributeKeyed(ViewAttributes.HeightAttribKey)
    /// Get the value of the LoadedEvent member
    member this.LoadedEvent = element.GetAttributeKeyed(ViewAttributes.LoadedEventAttribKey)

/// Viewer that allows to read the properties of a ViewElement representing a Panel
type PanelViewer(element: ViewElement) =
    inherit FrameworkElementViewer(element)
    do if not ((typeof<Windows.UI.Xaml.Controls.Panel>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'Windows.UI.Xaml.Controls.Panel' is expected, but '%s' was provided." element.TargetType.FullName

/// Viewer that allows to read the properties of a ViewElement representing a Grid
type GridViewer(element: ViewElement) =
    inherit PanelViewer(element)
    do if not ((typeof<Windows.UI.Xaml.Controls.Grid>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'Windows.UI.Xaml.Controls.Grid' is expected, but '%s' was provided." element.TargetType.FullName
    /// Get the value of the RowSpacing member
    member this.RowSpacing = element.GetAttributeKeyed(ViewAttributes.RowSpacingAttribKey)

/// Viewer that allows to read the properties of a ViewElement representing a StackPanel
type StackPanelViewer(element: ViewElement) =
    inherit PanelViewer(element)
    do if not ((typeof<Windows.UI.Xaml.Controls.StackPanel>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'Windows.UI.Xaml.Controls.StackPanel' is expected, but '%s' was provided." element.TargetType.FullName
    /// Get the value of the Orientation member
    member this.Orientation = element.GetAttributeKeyed(ViewAttributes.OrientationAttribKey)

/// Viewer that allows to read the properties of a ViewElement representing a TextBlock
type TextBlockViewer(element: ViewElement) =
    inherit FrameworkElementViewer(element)
    do if not ((typeof<Windows.UI.Xaml.Controls.TextBlock>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'Windows.UI.Xaml.Controls.TextBlock' is expected, but '%s' was provided." element.TargetType.FullName
    /// Get the value of the Text member
    member this.Text = element.GetAttributeKeyed(ViewAttributes.TextAttribKey)
    /// Get the value of the Foreground member
    member this.Foreground = element.GetAttributeKeyed(ViewAttributes.ForegroundAttribKey)

[<AbstractClass; Sealed>]
type View private () =
    /// Describes a FrameworkElement in the view
    static member inline FrameworkElement(?height: float,
                                          ?loadedEvent: obj -> unit,
                                          ?opacity: float,
                                          ?width: float) =

        ViewBuilders.ConstructFrameworkElement(?height=height,
                               ?loadedEvent=loadedEvent,
                               ?opacity=opacity,
                               ?width=width)

    /// Describes a Panel in the view
    static member inline Panel(?height: float,
                               ?loadedEvent: obj -> unit,
                               ?opacity: float,
                               ?width: float) =

        ViewBuilders.ConstructPanel(?height=height,
                               ?loadedEvent=loadedEvent,
                               ?opacity=opacity,
                               ?width=width)

    /// Describes a Grid in the view
    static member inline Grid(?height: float,
                              ?loadedEvent: obj -> unit,
                              ?opacity: float,
                              ?rowSpacing: float,
                              ?width: float) =

        ViewBuilders.ConstructGrid(?height=height,
                               ?loadedEvent=loadedEvent,
                               ?opacity=opacity,
                               ?rowSpacing=rowSpacing,
                               ?width=width)

    /// Describes a StackPanel in the view
    static member inline StackPanel(?height: float,
                                    ?loadedEvent: obj -> unit,
                                    ?opacity: float,
                                    ?orientation: Windows.UI.Xaml.Controls.Orientation,
                                    ?width: float) =

        ViewBuilders.ConstructStackPanel(?height=height,
                               ?loadedEvent=loadedEvent,
                               ?opacity=opacity,
                               ?orientation=orientation,
                               ?width=width)

    /// Describes a TextBlock in the view
    static member inline TextBlock(?foreground: Windows.UI.Xaml.Media.Brush,
                                   ?height: float,
                                   ?loadedEvent: obj -> unit,
                                   ?opacity: float,
                                   ?text: string,
                                   ?width: float) =

        ViewBuilders.ConstructTextBlock(?foreground=foreground,
                               ?height=height,
                               ?loadedEvent=loadedEvent,
                               ?opacity=opacity,
                               ?text=text,
                               ?width=width)


[<AutoOpen>]
module ViewElementExtensions = 

    type ViewElement with

        /// Adjusts the Opacity property in the visual element
        member x.Opacity(value: float) = x.WithAttribute(ViewAttributes.OpacityAttribKey, (value))

        /// Adjusts the LoadedEvent property in the visual element
        member x.LoadedEvent(value: obj -> unit) = x.WithAttribute(ViewAttributes.LoadedEventAttribKey, (fun f -> System.EventHandler<unit>(fun _sender _args -> f _args))(value))

        /// Adjusts the Width property in the visual element
        member x.Width(value: float) = x.WithAttribute(ViewAttributes.WidthAttribKey, (value))

        /// Adjusts the Height property in the visual element
        member x.Height(value: float) = x.WithAttribute(ViewAttributes.HeightAttribKey, (value))

        /// Adjusts the RowSpacing property in the visual element
        member x.RowSpacing(value: float) = x.WithAttribute(ViewAttributes.RowSpacingAttribKey, (value))

        /// Adjusts the Orientation property in the visual element
        member x.Orientation(value: Windows.UI.Xaml.Controls.Orientation) = x.WithAttribute(ViewAttributes.OrientationAttribKey, (value))

        /// Adjusts the Text property in the visual element
        member x.Text(value: string) = x.WithAttribute(ViewAttributes.TextAttribKey, (value))

        /// Adjusts the Foreground property in the visual element
        member x.Foreground(value: Windows.UI.Xaml.Media.Brush) = x.WithAttribute(ViewAttributes.ForegroundAttribKey, (value))

        member inline x.With(?opacity: float, ?loadedEvent: obj -> unit, ?width: float, ?height: float, ?rowSpacing: float, 
                             ?orientation: Windows.UI.Xaml.Controls.Orientation, ?text: string, ?foreground: Windows.UI.Xaml.Media.Brush) =
            let x = match opacity with None -> x | Some opt -> x.Opacity(opt)
            let x = match loadedEvent with None -> x | Some opt -> x.LoadedEvent(opt)
            let x = match width with None -> x | Some opt -> x.Width(opt)
            let x = match height with None -> x | Some opt -> x.Height(opt)
            let x = match rowSpacing with None -> x | Some opt -> x.RowSpacing(opt)
            let x = match orientation with None -> x | Some opt -> x.Orientation(opt)
            let x = match text with None -> x | Some opt -> x.Text(opt)
            let x = match foreground with None -> x | Some opt -> x.Foreground(opt)
            x

    /// Adjusts the Opacity property in the visual element
    let opacity (value: float) (x: ViewElement) = x.Opacity(value)
    /// Adjusts the LoadedEvent property in the visual element
    let loadedEvent (value: obj -> unit) (x: ViewElement) = x.LoadedEvent(value)
    /// Adjusts the Width property in the visual element
    let width (value: float) (x: ViewElement) = x.Width(value)
    /// Adjusts the Height property in the visual element
    let height (value: float) (x: ViewElement) = x.Height(value)
    /// Adjusts the RowSpacing property in the visual element
    let rowSpacing (value: float) (x: ViewElement) = x.RowSpacing(value)
    /// Adjusts the Orientation property in the visual element
    let orientation (value: Windows.UI.Xaml.Controls.Orientation) (x: ViewElement) = x.Orientation(value)
    /// Adjusts the Text property in the visual element
    let text (value: string) (x: ViewElement) = x.Text(value)
    /// Adjusts the Foreground property in the visual element
    let foreground (value: Windows.UI.Xaml.Media.Brush) (x: ViewElement) = x.Foreground(value)
