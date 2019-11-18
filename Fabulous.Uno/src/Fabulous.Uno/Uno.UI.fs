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
    let HorizontalAlignmentAttribKey : AttributeKey<_> = AttributeKey<_>("HorizontalAlignment")
    let VerticalAlignmentAttribKey : AttributeKey<_> = AttributeKey<_>("VerticalAlignment")
    let ContentAttribKey : AttributeKey<_> = AttributeKey<_>("Content")
    let CommandAttribKey : AttributeKey<_> = AttributeKey<_>("Command")
    let CommandCanExecuteAttribKey : AttributeKey<_> = AttributeKey<_>("CommandCanExecute")
    let ValueChangedAttribKey : AttributeKey<_> = AttributeKey<_>("ValueChanged")
    let ValueAttribKey : AttributeKey<_> = AttributeKey<_>("Value")
    let MinimumMaximumAttribKey : AttributeKey<_> = AttributeKey<_>("MinimumMaximum")
    let ToggledAttribKey : AttributeKey<_> = AttributeKey<_>("Toggled")
    let IsOnAttribKey : AttributeKey<_> = AttributeKey<_>("IsOn")
    let PaddingAttribKey : AttributeKey<_> = AttributeKey<_>("Padding")
    let ChildrenAttribKey : AttributeKey<_> = AttributeKey<_>("Children")
    let RowSpacingAttribKey : AttributeKey<_> = AttributeKey<_>("RowSpacing")
    let OrientationAttribKey : AttributeKey<_> = AttributeKey<_>("Orientation")
    let TextAttribKey : AttributeKey<_> = AttributeKey<_>("Text")
    let HorizontalTextAlignmentAttribKey : AttributeKey<_> = AttributeKey<_>("HorizontalTextAlignment")
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
                                               ?horizontalAlignment: Windows.UI.Xaml.HorizontalAlignment,
                                               ?verticalAlignment: Windows.UI.Xaml.VerticalAlignment,
                                               ?opacity: float,
                                               ?loadedEvent: obj -> unit) = 

        let attribCount = match width with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match height with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match horizontalAlignment with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match verticalAlignment with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match loadedEvent with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = ViewBuilders.BuildUIElement(attribCount, ?opacity=opacity)
        match width with None -> () | Some v -> attribBuilder.Add(ViewAttributes.WidthAttribKey, (v)) 
        match height with None -> () | Some v -> attribBuilder.Add(ViewAttributes.HeightAttribKey, (v)) 
        match horizontalAlignment with None -> () | Some v -> attribBuilder.Add(ViewAttributes.HorizontalAlignmentAttribKey, (v)) 
        match verticalAlignment with None -> () | Some v -> attribBuilder.Add(ViewAttributes.VerticalAlignmentAttribKey, (v)) 
        match loadedEvent with None -> () | Some v -> attribBuilder.Add(ViewAttributes.LoadedEventAttribKey, (fun f -> System.EventHandler<unit>(fun _sender _args -> f _args))(v)) 
        attribBuilder

    static member CreateFrameworkElement () : Windows.UI.Xaml.FrameworkElement =
        new Windows.UI.Xaml.FrameworkElement()

    static member UpdateFrameworkElement (prevOpt: ViewElement voption, curr: ViewElement, target: Windows.UI.Xaml.FrameworkElement) = 
        let mutable prevWidthOpt = ValueNone
        let mutable currWidthOpt = ValueNone
        let mutable prevHeightOpt = ValueNone
        let mutable currHeightOpt = ValueNone
        let mutable prevHorizontalAlignmentOpt = ValueNone
        let mutable currHorizontalAlignmentOpt = ValueNone
        let mutable prevVerticalAlignmentOpt = ValueNone
        let mutable currVerticalAlignmentOpt = ValueNone
        for kvp in curr.AttributesKeyed do
            if kvp.Key = ViewAttributes.WidthAttribKey.KeyValue then 
                currWidthOpt <- ValueSome (kvp.Value :?> float)
            if kvp.Key = ViewAttributes.HeightAttribKey.KeyValue then 
                currHeightOpt <- ValueSome (kvp.Value :?> float)
            if kvp.Key = ViewAttributes.HorizontalAlignmentAttribKey.KeyValue then 
                currHorizontalAlignmentOpt <- ValueSome (kvp.Value :?> Windows.UI.Xaml.HorizontalAlignment)
            if kvp.Key = ViewAttributes.VerticalAlignmentAttribKey.KeyValue then 
                currVerticalAlignmentOpt <- ValueSome (kvp.Value :?> Windows.UI.Xaml.VerticalAlignment)
        match prevOpt with
        | ValueNone -> ()
        | ValueSome prev ->
            for kvp in prev.AttributesKeyed do
                if kvp.Key = ViewAttributes.WidthAttribKey.KeyValue then 
                    prevWidthOpt <- ValueSome (kvp.Value :?> float)
                if kvp.Key = ViewAttributes.HeightAttribKey.KeyValue then 
                    prevHeightOpt <- ValueSome (kvp.Value :?> float)
                if kvp.Key = ViewAttributes.HorizontalAlignmentAttribKey.KeyValue then 
                    prevHorizontalAlignmentOpt <- ValueSome (kvp.Value :?> Windows.UI.Xaml.HorizontalAlignment)
                if kvp.Key = ViewAttributes.VerticalAlignmentAttribKey.KeyValue then 
                    prevVerticalAlignmentOpt <- ValueSome (kvp.Value :?> Windows.UI.Xaml.VerticalAlignment)
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
        match prevHorizontalAlignmentOpt, currHorizontalAlignmentOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.HorizontalAlignment <-  currValue
        | ValueSome _, ValueNone -> target.ClearValue Windows.UI.Xaml.FrameworkElement.HorizontalAlignmentProperty
        | ValueNone, ValueNone -> ()
        match prevVerticalAlignmentOpt, currVerticalAlignmentOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.VerticalAlignment <-  currValue
        | ValueSome _, ValueNone -> target.ClearValue Windows.UI.Xaml.FrameworkElement.VerticalAlignmentProperty
        | ValueNone, ValueNone -> ()

    static member inline ConstructFrameworkElement(?width: float,
                                                   ?height: float,
                                                   ?horizontalAlignment: Windows.UI.Xaml.HorizontalAlignment,
                                                   ?verticalAlignment: Windows.UI.Xaml.VerticalAlignment,
                                                   ?opacity: float,
                                                   ?loadedEvent: obj -> unit) = 

        let attribBuilder = ViewBuilders.BuildFrameworkElement(0,
                               ?width=width,
                               ?height=height,
                               ?horizontalAlignment=horizontalAlignment,
                               ?verticalAlignment=verticalAlignment,
                               ?opacity=opacity,
                               ?loadedEvent=loadedEvent)

        ViewElement.Create<Windows.UI.Xaml.FrameworkElement>(ViewBuilders.CreateFrameworkElement, (fun prevOpt curr target -> ViewBuilders.UpdateFrameworkElement(prevOpt, curr, target)), attribBuilder)

    /// Builds the attributes for a ContentControl in the view
    static member inline BuildContentControl(attribCount: int,
                                             ?content: obj,
                                             ?width: float,
                                             ?height: float,
                                             ?horizontalAlignment: Windows.UI.Xaml.HorizontalAlignment,
                                             ?verticalAlignment: Windows.UI.Xaml.VerticalAlignment,
                                             ?opacity: float,
                                             ?loadedEvent: obj -> unit) = 

        let attribCount = match content with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = ViewBuilders.BuildFrameworkElement(attribCount, ?width=width, ?height=height, ?horizontalAlignment=horizontalAlignment, ?verticalAlignment=verticalAlignment, ?opacity=opacity, 
                                                               ?loadedEvent=loadedEvent)
        match content with None -> () | Some v -> attribBuilder.Add(ViewAttributes.ContentAttribKey, (v)) 
        attribBuilder

    static member CreateContentControl () : Windows.UI.Xaml.Controls.ContentControl =
        new Windows.UI.Xaml.Controls.ContentControl()

    static member UpdateContentControl (prevOpt: ViewElement voption, curr: ViewElement, target: Windows.UI.Xaml.Controls.ContentControl) = 
        let mutable prevContentOpt = ValueNone
        let mutable currContentOpt = ValueNone
        for kvp in curr.AttributesKeyed do
            if kvp.Key = ViewAttributes.ContentAttribKey.KeyValue then 
                currContentOpt <- ValueSome (kvp.Value :?> obj)
        match prevOpt with
        | ValueNone -> ()
        | ValueSome prev ->
            for kvp in prev.AttributesKeyed do
                if kvp.Key = ViewAttributes.ContentAttribKey.KeyValue then 
                    prevContentOpt <- ValueSome (kvp.Value :?> obj)
        // Update inherited members
        ViewBuilders.UpdateFrameworkElement (prevOpt, curr, target)
        // Update properties
        match prevContentOpt, currContentOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.Content <-  currValue
        | ValueSome _, ValueNone -> target.ClearValue Windows.UI.Xaml.Controls.ContentControl.ContentProperty
        | ValueNone, ValueNone -> ()

    static member inline ConstructContentControl(?content: obj,
                                                 ?width: float,
                                                 ?height: float,
                                                 ?horizontalAlignment: Windows.UI.Xaml.HorizontalAlignment,
                                                 ?verticalAlignment: Windows.UI.Xaml.VerticalAlignment,
                                                 ?opacity: float,
                                                 ?loadedEvent: obj -> unit) = 

        let attribBuilder = ViewBuilders.BuildContentControl(0,
                               ?content=content,
                               ?width=width,
                               ?height=height,
                               ?horizontalAlignment=horizontalAlignment,
                               ?verticalAlignment=verticalAlignment,
                               ?opacity=opacity,
                               ?loadedEvent=loadedEvent)

        ViewElement.Create<Windows.UI.Xaml.Controls.ContentControl>(ViewBuilders.CreateContentControl, (fun prevOpt curr target -> ViewBuilders.UpdateContentControl(prevOpt, curr, target)), attribBuilder)

    /// Builds the attributes for a ButtonBase in the view
    static member inline BuildButtonBase(attribCount: int,
                                         ?command: unit -> unit,
                                         ?commandCanExecute: bool,
                                         ?content: obj,
                                         ?width: float,
                                         ?height: float,
                                         ?horizontalAlignment: Windows.UI.Xaml.HorizontalAlignment,
                                         ?verticalAlignment: Windows.UI.Xaml.VerticalAlignment,
                                         ?opacity: float,
                                         ?loadedEvent: obj -> unit) = 

        let attribCount = match command with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match commandCanExecute with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = ViewBuilders.BuildContentControl(attribCount, ?content=content, ?width=width, ?height=height, ?horizontalAlignment=horizontalAlignment, ?verticalAlignment=verticalAlignment, 
                                                             ?opacity=opacity, ?loadedEvent=loadedEvent)
        match command with None -> () | Some v -> attribBuilder.Add(ViewAttributes.CommandAttribKey, (v)) 
        match commandCanExecute with None -> () | Some v -> attribBuilder.Add(ViewAttributes.CommandCanExecuteAttribKey, (v)) 
        attribBuilder

    static member UpdateButtonBase (prevOpt: ViewElement voption, curr: ViewElement, target: Windows.UI.Xaml.Controls.Primitives.ButtonBase) = 
        let mutable prevCommandOpt = ValueNone
        let mutable currCommandOpt = ValueNone
        let mutable prevCommandCanExecuteOpt = ValueNone
        let mutable currCommandCanExecuteOpt = ValueNone
        for kvp in curr.AttributesKeyed do
            if kvp.Key = ViewAttributes.CommandAttribKey.KeyValue then 
                currCommandOpt <- ValueSome (kvp.Value :?> unit -> unit)
            if kvp.Key = ViewAttributes.CommandCanExecuteAttribKey.KeyValue then 
                currCommandCanExecuteOpt <- ValueSome (kvp.Value :?> bool)
        match prevOpt with
        | ValueNone -> ()
        | ValueSome prev ->
            for kvp in prev.AttributesKeyed do
                if kvp.Key = ViewAttributes.CommandAttribKey.KeyValue then 
                    prevCommandOpt <- ValueSome (kvp.Value :?> unit -> unit)
                if kvp.Key = ViewAttributes.CommandCanExecuteAttribKey.KeyValue then 
                    prevCommandCanExecuteOpt <- ValueSome (kvp.Value :?> bool)
        // Update inherited members
        ViewBuilders.UpdateContentControl (prevOpt, curr, target)
        // Update properties
        (fun _ _ _ -> ()) prevCommandOpt currCommandOpt target
        ViewUpdaters.updateCommand prevCommandOpt currCommandOpt (fun _target -> ()) (fun (target: Windows.UI.Xaml.Controls.Primitives.ButtonBase) cmd -> target.Command <- cmd) prevCommandCanExecuteOpt currCommandCanExecuteOpt target

    /// Builds the attributes for a Button in the view
    static member inline BuildButton(attribCount: int,
                                     ?command: unit -> unit,
                                     ?commandCanExecute: bool,
                                     ?content: obj,
                                     ?width: float,
                                     ?height: float,
                                     ?horizontalAlignment: Windows.UI.Xaml.HorizontalAlignment,
                                     ?verticalAlignment: Windows.UI.Xaml.VerticalAlignment,
                                     ?opacity: float,
                                     ?loadedEvent: obj -> unit) = 
        let attribBuilder = ViewBuilders.BuildButtonBase(attribCount, ?command=command, ?commandCanExecute=commandCanExecute, ?content=content, ?width=width, ?height=height, 
                                                         ?horizontalAlignment=horizontalAlignment, ?verticalAlignment=verticalAlignment, ?opacity=opacity, ?loadedEvent=loadedEvent)
        attribBuilder

    static member CreateButton () : Windows.UI.Xaml.Controls.Button =
        new Windows.UI.Xaml.Controls.Button()

    static member UpdateButton (prevOpt: ViewElement voption, curr: ViewElement, target: Windows.UI.Xaml.Controls.Button) = 
        ViewBuilders.UpdateButtonBase (prevOpt, curr, target)

    static member inline ConstructButton(?command: unit -> unit,
                                         ?commandCanExecute: bool,
                                         ?content: obj,
                                         ?width: float,
                                         ?height: float,
                                         ?horizontalAlignment: Windows.UI.Xaml.HorizontalAlignment,
                                         ?verticalAlignment: Windows.UI.Xaml.VerticalAlignment,
                                         ?opacity: float,
                                         ?loadedEvent: obj -> unit) = 

        let attribBuilder = ViewBuilders.BuildButton(0,
                               ?command=command,
                               ?commandCanExecute=commandCanExecute,
                               ?content=content,
                               ?width=width,
                               ?height=height,
                               ?horizontalAlignment=horizontalAlignment,
                               ?verticalAlignment=verticalAlignment,
                               ?opacity=opacity,
                               ?loadedEvent=loadedEvent)

        ViewElement.Create<Windows.UI.Xaml.Controls.Button>(ViewBuilders.CreateButton, (fun prevOpt curr target -> ViewBuilders.UpdateButton(prevOpt, curr, target)), attribBuilder)

    /// Builds the attributes for a RangeBase in the view
    static member inline BuildRangeBase(attribCount: int,
                                        ?value: float,
                                        ?minimumMaximum: double * double,
                                        ?width: float,
                                        ?height: float,
                                        ?horizontalAlignment: Windows.UI.Xaml.HorizontalAlignment,
                                        ?verticalAlignment: Windows.UI.Xaml.VerticalAlignment,
                                        ?opacity: float,
                                        ?valueChanged: double -> unit,
                                        ?loadedEvent: obj -> unit) = 

        let attribCount = match value with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match minimumMaximum with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match valueChanged with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = ViewBuilders.BuildFrameworkElement(attribCount, ?width=width, ?height=height, ?horizontalAlignment=horizontalAlignment, ?verticalAlignment=verticalAlignment, ?opacity=opacity, 
                                                               ?loadedEvent=loadedEvent)
        match value with None -> () | Some v -> attribBuilder.Add(ViewAttributes.ValueAttribKey, (v)) 
        match minimumMaximum with None -> () | Some v -> attribBuilder.Add(ViewAttributes.MinimumMaximumAttribKey, (v)) 
        match valueChanged with None -> () | Some v -> attribBuilder.Add(ViewAttributes.ValueChangedAttribKey, ViewConverters.makeValueChangedEventHandler(v)) 
        attribBuilder

    static member UpdateRangeBase (prevOpt: ViewElement voption, curr: ViewElement, target: Windows.UI.Xaml.Controls.Primitives.RangeBase) = 
        let mutable prevValueChangedOpt = ValueNone
        let mutable currValueChangedOpt = ValueNone
        let mutable prevValueOpt = ValueNone
        let mutable currValueOpt = ValueNone
        let mutable prevMinimumMaximumOpt = ValueNone
        let mutable currMinimumMaximumOpt = ValueNone
        for kvp in curr.AttributesKeyed do
            if kvp.Key = ViewAttributes.ValueChangedAttribKey.KeyValue then 
                currValueChangedOpt <- ValueSome (kvp.Value :?> Windows.UI.Xaml.Controls.Primitives.RangeBaseValueChangedEventHandler)
            if kvp.Key = ViewAttributes.ValueAttribKey.KeyValue then 
                currValueOpt <- ValueSome (kvp.Value :?> float)
            if kvp.Key = ViewAttributes.MinimumMaximumAttribKey.KeyValue then 
                currMinimumMaximumOpt <- ValueSome (kvp.Value :?> double * double)
        match prevOpt with
        | ValueNone -> ()
        | ValueSome prev ->
            for kvp in prev.AttributesKeyed do
                if kvp.Key = ViewAttributes.ValueChangedAttribKey.KeyValue then 
                    prevValueChangedOpt <- ValueSome (kvp.Value :?> Windows.UI.Xaml.Controls.Primitives.RangeBaseValueChangedEventHandler)
                if kvp.Key = ViewAttributes.ValueAttribKey.KeyValue then 
                    prevValueOpt <- ValueSome (kvp.Value :?> float)
                if kvp.Key = ViewAttributes.MinimumMaximumAttribKey.KeyValue then 
                    prevMinimumMaximumOpt <- ValueSome (kvp.Value :?> double * double)
        // Unsubscribe previous event handlers
        let shouldUpdateValueChanged = not ((identical prevValueChangedOpt currValueChangedOpt) && (identical prevValueOpt currValueOpt))
        if shouldUpdateValueChanged then
            match prevValueChangedOpt with
            | ValueSome prevValue -> target.ValueChanged.RemoveHandler(prevValue)
            | ValueNone -> ()
        // Update inherited members
        ViewBuilders.UpdateFrameworkElement (prevOpt, curr, target)
        // Update properties
        match prevValueOpt, currValueOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.Value <-  currValue
        | ValueSome _, ValueNone -> target.ClearValue Windows.UI.Xaml.Controls.Primitives.RangeBase.ValueProperty
        | ValueNone, ValueNone -> ()
        ViewUpdaters.updateRangeBaseMinimumMaximum prevMinimumMaximumOpt currMinimumMaximumOpt target
        // Subscribe new event handlers
        if shouldUpdateValueChanged then
            match currValueChangedOpt with
            | ValueSome currValue -> target.ValueChanged.AddHandler(currValue)
            | ValueNone -> ()

    /// Builds the attributes for a Slider in the view
    static member inline BuildSlider(attribCount: int,
                                     ?value: float,
                                     ?minimumMaximum: double * double,
                                     ?width: float,
                                     ?height: float,
                                     ?horizontalAlignment: Windows.UI.Xaml.HorizontalAlignment,
                                     ?verticalAlignment: Windows.UI.Xaml.VerticalAlignment,
                                     ?opacity: float,
                                     ?valueChanged: double -> unit,
                                     ?loadedEvent: obj -> unit) = 
        let attribBuilder = ViewBuilders.BuildRangeBase(attribCount, ?value=value, ?minimumMaximum=minimumMaximum, ?width=width, ?height=height, ?horizontalAlignment=horizontalAlignment, 
                                                        ?verticalAlignment=verticalAlignment, ?opacity=opacity, ?valueChanged=valueChanged, ?loadedEvent=loadedEvent)
        attribBuilder

    static member CreateSlider () : Windows.UI.Xaml.Controls.Slider =
        new Windows.UI.Xaml.Controls.Slider()

    static member UpdateSlider (prevOpt: ViewElement voption, curr: ViewElement, target: Windows.UI.Xaml.Controls.Slider) = 
        ViewBuilders.UpdateRangeBase (prevOpt, curr, target)

    static member inline ConstructSlider(?value: float,
                                         ?minimumMaximum: double * double,
                                         ?width: float,
                                         ?height: float,
                                         ?horizontalAlignment: Windows.UI.Xaml.HorizontalAlignment,
                                         ?verticalAlignment: Windows.UI.Xaml.VerticalAlignment,
                                         ?opacity: float,
                                         ?valueChanged: double -> unit,
                                         ?loadedEvent: obj -> unit) = 

        let attribBuilder = ViewBuilders.BuildSlider(0,
                               ?value=value,
                               ?minimumMaximum=minimumMaximum,
                               ?width=width,
                               ?height=height,
                               ?horizontalAlignment=horizontalAlignment,
                               ?verticalAlignment=verticalAlignment,
                               ?opacity=opacity,
                               ?valueChanged=valueChanged,
                               ?loadedEvent=loadedEvent)

        ViewElement.Create<Windows.UI.Xaml.Controls.Slider>(ViewBuilders.CreateSlider, (fun prevOpt curr target -> ViewBuilders.UpdateSlider(prevOpt, curr, target)), attribBuilder)

    /// Builds the attributes for a ToggleSwitch in the view
    static member inline BuildToggleSwitch(attribCount: int,
                                           ?isOn: bool,
                                           ?width: float,
                                           ?height: float,
                                           ?horizontalAlignment: Windows.UI.Xaml.HorizontalAlignment,
                                           ?verticalAlignment: Windows.UI.Xaml.VerticalAlignment,
                                           ?opacity: float,
                                           ?toggled: bool -> unit,
                                           ?loadedEvent: obj -> unit) = 

        let attribCount = match isOn with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match toggled with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = ViewBuilders.BuildFrameworkElement(attribCount, ?width=width, ?height=height, ?horizontalAlignment=horizontalAlignment, ?verticalAlignment=verticalAlignment, ?opacity=opacity, 
                                                               ?loadedEvent=loadedEvent)
        match isOn with None -> () | Some v -> attribBuilder.Add(ViewAttributes.IsOnAttribKey, (v)) 
        match toggled with None -> () | Some v -> attribBuilder.Add(ViewAttributes.ToggledAttribKey, ViewConverters.makeToggledEventHandler(v)) 
        attribBuilder

    static member CreateToggleSwitch () : Windows.UI.Xaml.Controls.ToggleSwitch =
        new Windows.UI.Xaml.Controls.ToggleSwitch()

    static member UpdateToggleSwitch (prevOpt: ViewElement voption, curr: ViewElement, target: Windows.UI.Xaml.Controls.ToggleSwitch) = 
        let mutable prevToggledOpt = ValueNone
        let mutable currToggledOpt = ValueNone
        let mutable prevIsOnOpt = ValueNone
        let mutable currIsOnOpt = ValueNone
        for kvp in curr.AttributesKeyed do
            if kvp.Key = ViewAttributes.ToggledAttribKey.KeyValue then 
                currToggledOpt <- ValueSome (kvp.Value :?> Windows.UI.Xaml.RoutedEventHandler)
            if kvp.Key = ViewAttributes.IsOnAttribKey.KeyValue then 
                currIsOnOpt <- ValueSome (kvp.Value :?> bool)
        match prevOpt with
        | ValueNone -> ()
        | ValueSome prev ->
            for kvp in prev.AttributesKeyed do
                if kvp.Key = ViewAttributes.ToggledAttribKey.KeyValue then 
                    prevToggledOpt <- ValueSome (kvp.Value :?> Windows.UI.Xaml.RoutedEventHandler)
                if kvp.Key = ViewAttributes.IsOnAttribKey.KeyValue then 
                    prevIsOnOpt <- ValueSome (kvp.Value :?> bool)
        // Unsubscribe previous event handlers
        let shouldUpdateToggled = not ((identical prevToggledOpt currToggledOpt) && (identical prevIsOnOpt currIsOnOpt))
        if shouldUpdateToggled then
            match prevToggledOpt with
            | ValueSome prevValue -> target.Toggled.RemoveHandler(prevValue)
            | ValueNone -> ()
        // Update inherited members
        ViewBuilders.UpdateFrameworkElement (prevOpt, curr, target)
        // Update properties
        match prevIsOnOpt, currIsOnOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.IsOn <-  currValue
        | ValueSome _, ValueNone -> target.ClearValue Windows.UI.Xaml.Controls.ToggleSwitch.IsOnProperty
        | ValueNone, ValueNone -> ()
        // Subscribe new event handlers
        if shouldUpdateToggled then
            match currToggledOpt with
            | ValueSome currValue -> target.Toggled.AddHandler(currValue)
            | ValueNone -> ()

    static member inline ConstructToggleSwitch(?isOn: bool,
                                               ?width: float,
                                               ?height: float,
                                               ?horizontalAlignment: Windows.UI.Xaml.HorizontalAlignment,
                                               ?verticalAlignment: Windows.UI.Xaml.VerticalAlignment,
                                               ?opacity: float,
                                               ?toggled: bool -> unit,
                                               ?loadedEvent: obj -> unit) = 

        let attribBuilder = ViewBuilders.BuildToggleSwitch(0,
                               ?isOn=isOn,
                               ?width=width,
                               ?height=height,
                               ?horizontalAlignment=horizontalAlignment,
                               ?verticalAlignment=verticalAlignment,
                               ?opacity=opacity,
                               ?toggled=toggled,
                               ?loadedEvent=loadedEvent)

        ViewElement.Create<Windows.UI.Xaml.Controls.ToggleSwitch>(ViewBuilders.CreateToggleSwitch, (fun prevOpt curr target -> ViewBuilders.UpdateToggleSwitch(prevOpt, curr, target)), attribBuilder)

    /// Builds the attributes for a Panel in the view
    static member inline BuildPanel(attribCount: int,
                                    ?padding: Windows.UI.Xaml.Thickness,
                                    ?children: ViewElement list,
                                    ?width: float,
                                    ?height: float,
                                    ?horizontalAlignment: Windows.UI.Xaml.HorizontalAlignment,
                                    ?verticalAlignment: Windows.UI.Xaml.VerticalAlignment,
                                    ?opacity: float,
                                    ?loadedEvent: obj -> unit) = 

        let attribCount = match padding with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match children with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = ViewBuilders.BuildFrameworkElement(attribCount, ?width=width, ?height=height, ?horizontalAlignment=horizontalAlignment, ?verticalAlignment=verticalAlignment, ?opacity=opacity, 
                                                               ?loadedEvent=loadedEvent)
        match padding with None -> () | Some v -> attribBuilder.Add(ViewAttributes.PaddingAttribKey, (v)) 
        match children with None -> () | Some v -> attribBuilder.Add(ViewAttributes.ChildrenAttribKey, Array.ofList(v)) 
        attribBuilder

    static member UpdatePanel (prevOpt: ViewElement voption, curr: ViewElement, target: Windows.UI.Xaml.Controls.Panel) = 
        let mutable prevPaddingOpt = ValueNone
        let mutable currPaddingOpt = ValueNone
        let mutable prevChildrenOpt = ValueNone
        let mutable currChildrenOpt = ValueNone
        for kvp in curr.AttributesKeyed do
            if kvp.Key = ViewAttributes.PaddingAttribKey.KeyValue then 
                currPaddingOpt <- ValueSome (kvp.Value :?> Windows.UI.Xaml.Thickness)
            if kvp.Key = ViewAttributes.ChildrenAttribKey.KeyValue then 
                currChildrenOpt <- ValueSome (kvp.Value :?> ViewElement array)
        match prevOpt with
        | ValueNone -> ()
        | ValueSome prev ->
            for kvp in prev.AttributesKeyed do
                if kvp.Key = ViewAttributes.PaddingAttribKey.KeyValue then 
                    prevPaddingOpt <- ValueSome (kvp.Value :?> Windows.UI.Xaml.Thickness)
                if kvp.Key = ViewAttributes.ChildrenAttribKey.KeyValue then 
                    prevChildrenOpt <- ValueSome (kvp.Value :?> ViewElement array)
        // Update inherited members
        ViewBuilders.UpdateFrameworkElement (prevOpt, curr, target)
        // Update properties
        match prevPaddingOpt, currPaddingOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.Padding <-  currValue
        | ValueSome _, ValueNone -> target.ClearValue Windows.UI.Xaml.Controls.Panel.PaddingProperty
        | ValueNone, ValueNone -> ()
        ViewUpdaters.updateCollectionGeneric prevChildrenOpt currChildrenOpt target.Children
            (fun (x:ViewElement) -> x.Create() :?> Windows.UI.Xaml.UIElement)
            (fun _ _ _ -> ())
            ViewHelpers.canReuseView
            ViewUpdaters.updateChild

    /// Builds the attributes for a Grid in the view
    static member inline BuildGrid(attribCount: int,
                                   ?rowSpacing: float,
                                   ?padding: Windows.UI.Xaml.Thickness,
                                   ?children: ViewElement list,
                                   ?width: float,
                                   ?height: float,
                                   ?horizontalAlignment: Windows.UI.Xaml.HorizontalAlignment,
                                   ?verticalAlignment: Windows.UI.Xaml.VerticalAlignment,
                                   ?opacity: float,
                                   ?loadedEvent: obj -> unit) = 

        let attribCount = match rowSpacing with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = ViewBuilders.BuildPanel(attribCount, ?padding=padding, ?children=children, ?width=width, ?height=height, ?horizontalAlignment=horizontalAlignment, 
                                                    ?verticalAlignment=verticalAlignment, ?opacity=opacity, ?loadedEvent=loadedEvent)
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
                                       ?padding: Windows.UI.Xaml.Thickness,
                                       ?children: ViewElement list,
                                       ?width: float,
                                       ?height: float,
                                       ?horizontalAlignment: Windows.UI.Xaml.HorizontalAlignment,
                                       ?verticalAlignment: Windows.UI.Xaml.VerticalAlignment,
                                       ?opacity: float,
                                       ?loadedEvent: obj -> unit) = 

        let attribBuilder = ViewBuilders.BuildGrid(0,
                               ?rowSpacing=rowSpacing,
                               ?padding=padding,
                               ?children=children,
                               ?width=width,
                               ?height=height,
                               ?horizontalAlignment=horizontalAlignment,
                               ?verticalAlignment=verticalAlignment,
                               ?opacity=opacity,
                               ?loadedEvent=loadedEvent)

        ViewElement.Create<Windows.UI.Xaml.Controls.Grid>(ViewBuilders.CreateGrid, (fun prevOpt curr target -> ViewBuilders.UpdateGrid(prevOpt, curr, target)), attribBuilder)

    /// Builds the attributes for a StackPanel in the view
    static member inline BuildStackPanel(attribCount: int,
                                         ?orientation: Windows.UI.Xaml.Controls.Orientation,
                                         ?padding: Windows.UI.Xaml.Thickness,
                                         ?children: ViewElement list,
                                         ?width: float,
                                         ?height: float,
                                         ?horizontalAlignment: Windows.UI.Xaml.HorizontalAlignment,
                                         ?verticalAlignment: Windows.UI.Xaml.VerticalAlignment,
                                         ?opacity: float,
                                         ?loadedEvent: obj -> unit) = 

        let attribCount = match orientation with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = ViewBuilders.BuildPanel(attribCount, ?padding=padding, ?children=children, ?width=width, ?height=height, ?horizontalAlignment=horizontalAlignment, 
                                                    ?verticalAlignment=verticalAlignment, ?opacity=opacity, ?loadedEvent=loadedEvent)
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
                                             ?padding: Windows.UI.Xaml.Thickness,
                                             ?children: ViewElement list,
                                             ?width: float,
                                             ?height: float,
                                             ?horizontalAlignment: Windows.UI.Xaml.HorizontalAlignment,
                                             ?verticalAlignment: Windows.UI.Xaml.VerticalAlignment,
                                             ?opacity: float,
                                             ?loadedEvent: obj -> unit) = 

        let attribBuilder = ViewBuilders.BuildStackPanel(0,
                               ?orientation=orientation,
                               ?padding=padding,
                               ?children=children,
                               ?width=width,
                               ?height=height,
                               ?horizontalAlignment=horizontalAlignment,
                               ?verticalAlignment=verticalAlignment,
                               ?opacity=opacity,
                               ?loadedEvent=loadedEvent)

        ViewElement.Create<Windows.UI.Xaml.Controls.StackPanel>(ViewBuilders.CreateStackPanel, (fun prevOpt curr target -> ViewBuilders.UpdateStackPanel(prevOpt, curr, target)), attribBuilder)

    /// Builds the attributes for a TextBlock in the view
    static member inline BuildTextBlock(attribCount: int,
                                        ?text: string,
                                        ?horizontalTextAlignment: Windows.UI.Xaml.TextAlignment,
                                        ?foreground: Windows.UI.Xaml.Media.Brush,
                                        ?width: float,
                                        ?height: float,
                                        ?horizontalAlignment: Windows.UI.Xaml.HorizontalAlignment,
                                        ?verticalAlignment: Windows.UI.Xaml.VerticalAlignment,
                                        ?opacity: float,
                                        ?loadedEvent: obj -> unit) = 

        let attribCount = match text with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match horizontalTextAlignment with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match foreground with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = ViewBuilders.BuildFrameworkElement(attribCount, ?width=width, ?height=height, ?horizontalAlignment=horizontalAlignment, ?verticalAlignment=verticalAlignment, ?opacity=opacity, 
                                                               ?loadedEvent=loadedEvent)
        match text with None -> () | Some v -> attribBuilder.Add(ViewAttributes.TextAttribKey, (v)) 
        match horizontalTextAlignment with None -> () | Some v -> attribBuilder.Add(ViewAttributes.HorizontalTextAlignmentAttribKey, (v)) 
        match foreground with None -> () | Some v -> attribBuilder.Add(ViewAttributes.ForegroundAttribKey, (v)) 
        attribBuilder

    static member CreateTextBlock () : Windows.UI.Xaml.Controls.TextBlock =
        new Windows.UI.Xaml.Controls.TextBlock()

    static member UpdateTextBlock (prevOpt: ViewElement voption, curr: ViewElement, target: Windows.UI.Xaml.Controls.TextBlock) = 
        let mutable prevTextOpt = ValueNone
        let mutable currTextOpt = ValueNone
        let mutable prevHorizontalTextAlignmentOpt = ValueNone
        let mutable currHorizontalTextAlignmentOpt = ValueNone
        let mutable prevForegroundOpt = ValueNone
        let mutable currForegroundOpt = ValueNone
        for kvp in curr.AttributesKeyed do
            if kvp.Key = ViewAttributes.TextAttribKey.KeyValue then 
                currTextOpt <- ValueSome (kvp.Value :?> string)
            if kvp.Key = ViewAttributes.HorizontalTextAlignmentAttribKey.KeyValue then 
                currHorizontalTextAlignmentOpt <- ValueSome (kvp.Value :?> Windows.UI.Xaml.TextAlignment)
            if kvp.Key = ViewAttributes.ForegroundAttribKey.KeyValue then 
                currForegroundOpt <- ValueSome (kvp.Value :?> Windows.UI.Xaml.Media.Brush)
        match prevOpt with
        | ValueNone -> ()
        | ValueSome prev ->
            for kvp in prev.AttributesKeyed do
                if kvp.Key = ViewAttributes.TextAttribKey.KeyValue then 
                    prevTextOpt <- ValueSome (kvp.Value :?> string)
                if kvp.Key = ViewAttributes.HorizontalTextAlignmentAttribKey.KeyValue then 
                    prevHorizontalTextAlignmentOpt <- ValueSome (kvp.Value :?> Windows.UI.Xaml.TextAlignment)
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
        match prevHorizontalTextAlignmentOpt, currHorizontalTextAlignmentOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.HorizontalTextAlignment <-  currValue
        | ValueSome _, ValueNone -> target.ClearValue Windows.UI.Xaml.Controls.TextBlock.HorizontalTextAlignmentProperty
        | ValueNone, ValueNone -> ()
        match prevForegroundOpt, currForegroundOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.Foreground <-  currValue
        | ValueSome _, ValueNone -> target.ClearValue Windows.UI.Xaml.Controls.TextBlock.ForegroundProperty
        | ValueNone, ValueNone -> ()

    static member inline ConstructTextBlock(?text: string,
                                            ?horizontalTextAlignment: Windows.UI.Xaml.TextAlignment,
                                            ?foreground: Windows.UI.Xaml.Media.Brush,
                                            ?width: float,
                                            ?height: float,
                                            ?horizontalAlignment: Windows.UI.Xaml.HorizontalAlignment,
                                            ?verticalAlignment: Windows.UI.Xaml.VerticalAlignment,
                                            ?opacity: float,
                                            ?loadedEvent: obj -> unit) = 

        let attribBuilder = ViewBuilders.BuildTextBlock(0,
                               ?text=text,
                               ?horizontalTextAlignment=horizontalTextAlignment,
                               ?foreground=foreground,
                               ?width=width,
                               ?height=height,
                               ?horizontalAlignment=horizontalAlignment,
                               ?verticalAlignment=verticalAlignment,
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
    /// Get the value of the HorizontalAlignment member
    member this.HorizontalAlignment = element.GetAttributeKeyed(ViewAttributes.HorizontalAlignmentAttribKey)
    /// Get the value of the VerticalAlignment member
    member this.VerticalAlignment = element.GetAttributeKeyed(ViewAttributes.VerticalAlignmentAttribKey)
    /// Get the value of the LoadedEvent member
    member this.LoadedEvent = element.GetAttributeKeyed(ViewAttributes.LoadedEventAttribKey)

/// Viewer that allows to read the properties of a ViewElement representing a ContentControl
type ContentControlViewer(element: ViewElement) =
    inherit FrameworkElementViewer(element)
    do if not ((typeof<Windows.UI.Xaml.Controls.ContentControl>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'Windows.UI.Xaml.Controls.ContentControl' is expected, but '%s' was provided." element.TargetType.FullName
    /// Get the value of the Content member
    member this.Content = element.GetAttributeKeyed(ViewAttributes.ContentAttribKey)

/// Viewer that allows to read the properties of a ViewElement representing a ButtonBase
type ButtonBaseViewer(element: ViewElement) =
    inherit ContentControlViewer(element)
    do if not ((typeof<Windows.UI.Xaml.Controls.Primitives.ButtonBase>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'Windows.UI.Xaml.Controls.Primitives.ButtonBase' is expected, but '%s' was provided." element.TargetType.FullName
    /// Get the value of the Command member
    member this.Command = element.GetAttributeKeyed(ViewAttributes.CommandAttribKey)
    /// Get the value of the CommandCanExecute member
    member this.CommandCanExecute = element.GetAttributeKeyed(ViewAttributes.CommandCanExecuteAttribKey)

/// Viewer that allows to read the properties of a ViewElement representing a Button
type ButtonViewer(element: ViewElement) =
    inherit ButtonBaseViewer(element)
    do if not ((typeof<Windows.UI.Xaml.Controls.Button>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'Windows.UI.Xaml.Controls.Button' is expected, but '%s' was provided." element.TargetType.FullName

/// Viewer that allows to read the properties of a ViewElement representing a RangeBase
type RangeBaseViewer(element: ViewElement) =
    inherit FrameworkElementViewer(element)
    do if not ((typeof<Windows.UI.Xaml.Controls.Primitives.RangeBase>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'Windows.UI.Xaml.Controls.Primitives.RangeBase' is expected, but '%s' was provided." element.TargetType.FullName
    /// Get the value of the Value member
    member this.Value = element.GetAttributeKeyed(ViewAttributes.ValueAttribKey)
    /// Get the value of the MinimumMaximum member
    member this.MinimumMaximum = element.GetAttributeKeyed(ViewAttributes.MinimumMaximumAttribKey)
    /// Get the value of the ValueChanged member
    member this.ValueChanged = element.GetAttributeKeyed(ViewAttributes.ValueChangedAttribKey)

/// Viewer that allows to read the properties of a ViewElement representing a Slider
type SliderViewer(element: ViewElement) =
    inherit RangeBaseViewer(element)
    do if not ((typeof<Windows.UI.Xaml.Controls.Slider>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'Windows.UI.Xaml.Controls.Slider' is expected, but '%s' was provided." element.TargetType.FullName

/// Viewer that allows to read the properties of a ViewElement representing a ToggleSwitch
type ToggleSwitchViewer(element: ViewElement) =
    inherit FrameworkElementViewer(element)
    do if not ((typeof<Windows.UI.Xaml.Controls.ToggleSwitch>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'Windows.UI.Xaml.Controls.ToggleSwitch' is expected, but '%s' was provided." element.TargetType.FullName
    /// Get the value of the IsOn member
    member this.IsOn = element.GetAttributeKeyed(ViewAttributes.IsOnAttribKey)
    /// Get the value of the Toggled member
    member this.Toggled = element.GetAttributeKeyed(ViewAttributes.ToggledAttribKey)

/// Viewer that allows to read the properties of a ViewElement representing a Panel
type PanelViewer(element: ViewElement) =
    inherit FrameworkElementViewer(element)
    do if not ((typeof<Windows.UI.Xaml.Controls.Panel>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'Windows.UI.Xaml.Controls.Panel' is expected, but '%s' was provided." element.TargetType.FullName
    /// Get the value of the Padding member
    member this.Padding = element.GetAttributeKeyed(ViewAttributes.PaddingAttribKey)
    /// Get the value of the Children member
    member this.Children = element.GetAttributeKeyed(ViewAttributes.ChildrenAttribKey)

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
    /// Get the value of the HorizontalTextAlignment member
    member this.HorizontalTextAlignment = element.GetAttributeKeyed(ViewAttributes.HorizontalTextAlignmentAttribKey)
    /// Get the value of the Foreground member
    member this.Foreground = element.GetAttributeKeyed(ViewAttributes.ForegroundAttribKey)

[<AbstractClass; Sealed>]
type View private () =
    /// Describes a FrameworkElement in the view
    static member inline FrameworkElement(?height: float,
                                          ?horizontalAlignment: Windows.UI.Xaml.HorizontalAlignment,
                                          ?loadedEvent: obj -> unit,
                                          ?opacity: float,
                                          ?verticalAlignment: Windows.UI.Xaml.VerticalAlignment,
                                          ?width: float) =

        ViewBuilders.ConstructFrameworkElement(?height=height,
                               ?horizontalAlignment=horizontalAlignment,
                               ?loadedEvent=loadedEvent,
                               ?opacity=opacity,
                               ?verticalAlignment=verticalAlignment,
                               ?width=width)

    /// Describes a ContentControl in the view
    static member inline ContentControl(?content: obj,
                                        ?height: float,
                                        ?horizontalAlignment: Windows.UI.Xaml.HorizontalAlignment,
                                        ?loadedEvent: obj -> unit,
                                        ?opacity: float,
                                        ?verticalAlignment: Windows.UI.Xaml.VerticalAlignment,
                                        ?width: float) =

        ViewBuilders.ConstructContentControl(?content=content,
                               ?height=height,
                               ?horizontalAlignment=horizontalAlignment,
                               ?loadedEvent=loadedEvent,
                               ?opacity=opacity,
                               ?verticalAlignment=verticalAlignment,
                               ?width=width)

    /// Describes a Button in the view
    static member inline Button(?command: unit -> unit,
                                ?commandCanExecute: bool,
                                ?content: obj,
                                ?height: float,
                                ?horizontalAlignment: Windows.UI.Xaml.HorizontalAlignment,
                                ?loadedEvent: obj -> unit,
                                ?opacity: float,
                                ?verticalAlignment: Windows.UI.Xaml.VerticalAlignment,
                                ?width: float) =

        ViewBuilders.ConstructButton(?command=command,
                               ?commandCanExecute=commandCanExecute,
                               ?content=content,
                               ?height=height,
                               ?horizontalAlignment=horizontalAlignment,
                               ?loadedEvent=loadedEvent,
                               ?opacity=opacity,
                               ?verticalAlignment=verticalAlignment,
                               ?width=width)

    /// Describes a Slider in the view
    static member inline Slider(?value: float,
                                ?height: float,
                                ?horizontalAlignment: Windows.UI.Xaml.HorizontalAlignment,
                                ?loadedEvent: obj -> unit,
                                ?minimumMaximum: double * double,
                                ?opacity: float,
                                ?valueChanged: double -> unit,
                                ?verticalAlignment: Windows.UI.Xaml.VerticalAlignment,
                                ?width: float) =

        ViewBuilders.ConstructSlider(?value=value,
                               ?height=height,
                               ?horizontalAlignment=horizontalAlignment,
                               ?loadedEvent=loadedEvent,
                               ?minimumMaximum=minimumMaximum,
                               ?opacity=opacity,
                               ?valueChanged=valueChanged,
                               ?verticalAlignment=verticalAlignment,
                               ?width=width)

    /// Describes a ToggleSwitch in the view
    static member inline ToggleSwitch(?isOn: bool,
                                      ?height: float,
                                      ?horizontalAlignment: Windows.UI.Xaml.HorizontalAlignment,
                                      ?loadedEvent: obj -> unit,
                                      ?opacity: float,
                                      ?toggled: bool -> unit,
                                      ?verticalAlignment: Windows.UI.Xaml.VerticalAlignment,
                                      ?width: float) =

        ViewBuilders.ConstructToggleSwitch(?isOn=isOn,
                               ?height=height,
                               ?horizontalAlignment=horizontalAlignment,
                               ?loadedEvent=loadedEvent,
                               ?opacity=opacity,
                               ?toggled=toggled,
                               ?verticalAlignment=verticalAlignment,
                               ?width=width)

    /// Describes a Grid in the view
    static member inline Grid(?children: ViewElement list,
                              ?height: float,
                              ?horizontalAlignment: Windows.UI.Xaml.HorizontalAlignment,
                              ?loadedEvent: obj -> unit,
                              ?opacity: float,
                              ?padding: Windows.UI.Xaml.Thickness,
                              ?rowSpacing: float,
                              ?verticalAlignment: Windows.UI.Xaml.VerticalAlignment,
                              ?width: float) =

        ViewBuilders.ConstructGrid(?children=children,
                               ?height=height,
                               ?horizontalAlignment=horizontalAlignment,
                               ?loadedEvent=loadedEvent,
                               ?opacity=opacity,
                               ?padding=padding,
                               ?rowSpacing=rowSpacing,
                               ?verticalAlignment=verticalAlignment,
                               ?width=width)

    /// Describes a StackPanel in the view
    static member inline StackPanel(?children: ViewElement list,
                                    ?height: float,
                                    ?horizontalAlignment: Windows.UI.Xaml.HorizontalAlignment,
                                    ?loadedEvent: obj -> unit,
                                    ?opacity: float,
                                    ?orientation: Windows.UI.Xaml.Controls.Orientation,
                                    ?padding: Windows.UI.Xaml.Thickness,
                                    ?verticalAlignment: Windows.UI.Xaml.VerticalAlignment,
                                    ?width: float) =

        ViewBuilders.ConstructStackPanel(?children=children,
                               ?height=height,
                               ?horizontalAlignment=horizontalAlignment,
                               ?loadedEvent=loadedEvent,
                               ?opacity=opacity,
                               ?orientation=orientation,
                               ?padding=padding,
                               ?verticalAlignment=verticalAlignment,
                               ?width=width)

    /// Describes a TextBlock in the view
    static member inline TextBlock(?foreground: Windows.UI.Xaml.Media.Brush,
                                   ?height: float,
                                   ?horizontalAlignment: Windows.UI.Xaml.HorizontalAlignment,
                                   ?horizontalTextAlignment: Windows.UI.Xaml.TextAlignment,
                                   ?loadedEvent: obj -> unit,
                                   ?opacity: float,
                                   ?text: string,
                                   ?verticalAlignment: Windows.UI.Xaml.VerticalAlignment,
                                   ?width: float) =

        ViewBuilders.ConstructTextBlock(?foreground=foreground,
                               ?height=height,
                               ?horizontalAlignment=horizontalAlignment,
                               ?horizontalTextAlignment=horizontalTextAlignment,
                               ?loadedEvent=loadedEvent,
                               ?opacity=opacity,
                               ?text=text,
                               ?verticalAlignment=verticalAlignment,
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

        /// Adjusts the HorizontalAlignment property in the visual element
        member x.HorizontalAlignment(value: Windows.UI.Xaml.HorizontalAlignment) = x.WithAttribute(ViewAttributes.HorizontalAlignmentAttribKey, (value))

        /// Adjusts the VerticalAlignment property in the visual element
        member x.VerticalAlignment(value: Windows.UI.Xaml.VerticalAlignment) = x.WithAttribute(ViewAttributes.VerticalAlignmentAttribKey, (value))

        /// Adjusts the Content property in the visual element
        member x.Content(value: obj) = x.WithAttribute(ViewAttributes.ContentAttribKey, (value))

        /// Adjusts the Command property in the visual element
        member x.Command(value: unit -> unit) = x.WithAttribute(ViewAttributes.CommandAttribKey, (value))

        /// Adjusts the CommandCanExecute property in the visual element
        member x.CommandCanExecute(value: bool) = x.WithAttribute(ViewAttributes.CommandCanExecuteAttribKey, (value))

        /// Adjusts the ValueChanged property in the visual element
        member x.ValueChanged(value: double -> unit) = x.WithAttribute(ViewAttributes.ValueChangedAttribKey, ViewConverters.makeValueChangedEventHandler(value))

        /// Adjusts the Value property in the visual element
        member x.Value(value: float) = x.WithAttribute(ViewAttributes.ValueAttribKey, (value))

        /// Adjusts the MinimumMaximum property in the visual element
        member x.MinimumMaximum(value: double * double) = x.WithAttribute(ViewAttributes.MinimumMaximumAttribKey, (value))

        /// Adjusts the Toggled property in the visual element
        member x.Toggled(value: bool -> unit) = x.WithAttribute(ViewAttributes.ToggledAttribKey, ViewConverters.makeToggledEventHandler(value))

        /// Adjusts the IsOn property in the visual element
        member x.IsOn(value: bool) = x.WithAttribute(ViewAttributes.IsOnAttribKey, (value))

        /// Adjusts the Padding property in the visual element
        member x.Padding(value: Windows.UI.Xaml.Thickness) = x.WithAttribute(ViewAttributes.PaddingAttribKey, (value))

        /// Adjusts the Children property in the visual element
        member x.Children(value: ViewElement list) = x.WithAttribute(ViewAttributes.ChildrenAttribKey, Array.ofList(value))

        /// Adjusts the RowSpacing property in the visual element
        member x.RowSpacing(value: float) = x.WithAttribute(ViewAttributes.RowSpacingAttribKey, (value))

        /// Adjusts the Orientation property in the visual element
        member x.Orientation(value: Windows.UI.Xaml.Controls.Orientation) = x.WithAttribute(ViewAttributes.OrientationAttribKey, (value))

        /// Adjusts the Text property in the visual element
        member x.Text(value: string) = x.WithAttribute(ViewAttributes.TextAttribKey, (value))

        /// Adjusts the HorizontalTextAlignment property in the visual element
        member x.HorizontalTextAlignment(value: Windows.UI.Xaml.TextAlignment) = x.WithAttribute(ViewAttributes.HorizontalTextAlignmentAttribKey, (value))

        /// Adjusts the Foreground property in the visual element
        member x.Foreground(value: Windows.UI.Xaml.Media.Brush) = x.WithAttribute(ViewAttributes.ForegroundAttribKey, (value))

        member inline x.With(?opacity: float, ?loadedEvent: obj -> unit, ?width: float, ?height: float, ?horizontalAlignment: Windows.UI.Xaml.HorizontalAlignment, 
                             ?verticalAlignment: Windows.UI.Xaml.VerticalAlignment, ?content: obj, ?command: unit -> unit, ?commandCanExecute: bool, ?valueChanged: double -> unit, 
                             ?value: float, ?minimumMaximum: double * double, ?toggled: bool -> unit, ?isOn: bool, ?padding: Windows.UI.Xaml.Thickness, 
                             ?children: ViewElement list, ?rowSpacing: float, ?orientation: Windows.UI.Xaml.Controls.Orientation, ?text: string, ?horizontalTextAlignment: Windows.UI.Xaml.TextAlignment, 
                             ?foreground: Windows.UI.Xaml.Media.Brush) =
            let x = match opacity with None -> x | Some opt -> x.Opacity(opt)
            let x = match loadedEvent with None -> x | Some opt -> x.LoadedEvent(opt)
            let x = match width with None -> x | Some opt -> x.Width(opt)
            let x = match height with None -> x | Some opt -> x.Height(opt)
            let x = match horizontalAlignment with None -> x | Some opt -> x.HorizontalAlignment(opt)
            let x = match verticalAlignment with None -> x | Some opt -> x.VerticalAlignment(opt)
            let x = match content with None -> x | Some opt -> x.Content(opt)
            let x = match command with None -> x | Some opt -> x.Command(opt)
            let x = match commandCanExecute with None -> x | Some opt -> x.CommandCanExecute(opt)
            let x = match valueChanged with None -> x | Some opt -> x.ValueChanged(opt)
            let x = match value with None -> x | Some opt -> x.Value(opt)
            let x = match minimumMaximum with None -> x | Some opt -> x.MinimumMaximum(opt)
            let x = match toggled with None -> x | Some opt -> x.Toggled(opt)
            let x = match isOn with None -> x | Some opt -> x.IsOn(opt)
            let x = match padding with None -> x | Some opt -> x.Padding(opt)
            let x = match children with None -> x | Some opt -> x.Children(opt)
            let x = match rowSpacing with None -> x | Some opt -> x.RowSpacing(opt)
            let x = match orientation with None -> x | Some opt -> x.Orientation(opt)
            let x = match text with None -> x | Some opt -> x.Text(opt)
            let x = match horizontalTextAlignment with None -> x | Some opt -> x.HorizontalTextAlignment(opt)
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
    /// Adjusts the HorizontalAlignment property in the visual element
    let horizontalAlignment (value: Windows.UI.Xaml.HorizontalAlignment) (x: ViewElement) = x.HorizontalAlignment(value)
    /// Adjusts the VerticalAlignment property in the visual element
    let verticalAlignment (value: Windows.UI.Xaml.VerticalAlignment) (x: ViewElement) = x.VerticalAlignment(value)
    /// Adjusts the Content property in the visual element
    let content (value: obj) (x: ViewElement) = x.Content(value)
    /// Adjusts the Command property in the visual element
    let command (value: unit -> unit) (x: ViewElement) = x.Command(value)
    /// Adjusts the CommandCanExecute property in the visual element
    let commandCanExecute (value: bool) (x: ViewElement) = x.CommandCanExecute(value)
    /// Adjusts the ValueChanged property in the visual element
    let valueChanged (value: double -> unit) (x: ViewElement) = x.ValueChanged(value)
    /// Adjusts the Value property in the visual element
    let value (value: float) (x: ViewElement) = x.Value(value)
    /// Adjusts the MinimumMaximum property in the visual element
    let minimumMaximum (value: double * double) (x: ViewElement) = x.MinimumMaximum(value)
    /// Adjusts the Toggled property in the visual element
    let toggled (value: bool -> unit) (x: ViewElement) = x.Toggled(value)
    /// Adjusts the IsOn property in the visual element
    let isOn (value: bool) (x: ViewElement) = x.IsOn(value)
    /// Adjusts the Padding property in the visual element
    let padding (value: Windows.UI.Xaml.Thickness) (x: ViewElement) = x.Padding(value)
    /// Adjusts the Children property in the visual element
    let children (value: ViewElement list) (x: ViewElement) = x.Children(value)
    /// Adjusts the RowSpacing property in the visual element
    let rowSpacing (value: float) (x: ViewElement) = x.RowSpacing(value)
    /// Adjusts the Orientation property in the visual element
    let orientation (value: Windows.UI.Xaml.Controls.Orientation) (x: ViewElement) = x.Orientation(value)
    /// Adjusts the Text property in the visual element
    let text (value: string) (x: ViewElement) = x.Text(value)
    /// Adjusts the HorizontalTextAlignment property in the visual element
    let horizontalTextAlignment (value: Windows.UI.Xaml.TextAlignment) (x: ViewElement) = x.HorizontalTextAlignment(value)
    /// Adjusts the Foreground property in the visual element
    let foreground (value: Windows.UI.Xaml.Media.Brush) (x: ViewElement) = x.Foreground(value)
