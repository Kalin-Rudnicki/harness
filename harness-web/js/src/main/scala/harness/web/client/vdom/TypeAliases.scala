package harness.web.client.vdom

type CModifier = PModifier[Nothing, Any, Nothing]
type CModifierA[+A] = PModifier[A, Any, Nothing]
type Modifier[S] = PModifier[Nothing, S, S]
type ModifierA[+A, S] = PModifier[A, S, S]

type CWidget = PWidget[Nothing, Any, Nothing, Unit]
type CWidgetA[+A] = PWidget[A, Any, Nothing, Unit]
type CWidgetV[+V] = PWidget[Nothing, Any, Nothing, V]
type CWidgetAV[+A, +V] = PWidget[A, Any, Nothing, V]
type Widget[S] = PWidget[Nothing, S, S, Unit]
type WidgetA[+A, S] = PWidget[A, S, S, Unit]
type WidgetV[S, +V] = PWidget[Nothing, S, S, V]
type WidgetAV[+A, S, +V] = PWidget[A, S, S, V]

type CNodeWidget = PNodeWidget[Nothing, Any, Nothing, Unit]
type CNodeWidgetA[+A] = PNodeWidget[A, Any, Nothing, Unit]
type CNodeWidgetV[+V] = PNodeWidget[Nothing, Any, Nothing, V]
type CNodeWidgetAV[+A, +V] = PNodeWidget[A, Any, Nothing, V]
type NodeWidget[S] = PNodeWidget[Nothing, S, S, Unit]
type NodeWidgetA[+A, S] = PNodeWidget[A, S, S, Unit]
type NodeWidgetV[S, +V] = PNodeWidget[Nothing, S, S, V]
type NodeWidgetAV[+A, S, +V] = PNodeWidget[A, S, S, V]
