package harness.webUI.vdom

type CModifier = PModifier[Nothing, Any, Nothing, Unit]
type CModifierA[+A] = PModifier[A, Any, Nothing, Unit]
type CModifierV[+V] = PModifier[Nothing, Any, Nothing, V]
type CModifierAV[+A, +V] = PModifier[A, Any, Nothing, V]
type Modifier[S] = PModifier[Nothing, S, S, Unit]
type ModifierA[+A, S] = PModifier[A, S, S, Unit]
type ModifierV[S, +V] = PModifier[Nothing, S, S, V]
type ModifierAV[+A, S, +V] = PModifier[A, S, S, V]

type CNodeWidget = PNodeWidget[Nothing, Any, Nothing, Unit]
type CNodeWidgetA[+A] = PNodeWidget[A, Any, Nothing, Unit]
type CNodeWidgetV[+V] = PNodeWidget[Nothing, Any, Nothing, V]
type CNodeWidgetAV[+A, +V] = PNodeWidget[A, Any, Nothing, V]
type NodeWidget[S] = PNodeWidget[Nothing, S, S, Unit]
type NodeWidgetA[+A, S] = PNodeWidget[A, S, S, Unit]
type NodeWidgetV[S, +V] = PNodeWidget[Nothing, S, S, V]
type NodeWidgetAV[+A, S, +V] = PNodeWidget[A, S, S, V]
