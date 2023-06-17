package harness.webUI.style

import harness.webUI.rawVDOM.VDom.CSSAttr
import harness.webUI.style.StyleSheetUtils.*
import harness.webUI.vdom.{given, *}

abstract class _DefaultStyleSheet(
    navBarHeight: String = 2.rem,
)(implicit colorPalate: ColorPalate)
    extends StyleSheet("default harness style-sheet")(
      inTag("*")(
        margin := "0",
        padding := "0",
        boxSizing.borderBox,
      ),
      inTag("h1")(
        color := colorSecondary,
        marginLeft := 2.rem,
        marginBottom := 1.rem,
      ),
    ) {

  object page
      extends Block("page")(
        bgBackground,
        height := s"calc(100vh - $navBarHeight)",
        padding := "0",
        margin := "0",
      ) {

    object body
        extends Element("body")(
          padding := "2rem 5%",
        ); body

  }; page

  object navBar
      extends Block("nav-bar")(
        bgPrimary,
        display.flex,
        flexWrap.nowrap,
        height := navBarHeight,
      ) {

    object section
        extends Element("section")(
          height := 100.pct,
        ) {

      object wrap
          extends Modifier("wrap")(
            flex := "0 1 auto",
          ); wrap

      object expand
          extends Modifier("expand")(
            flex := "1 0 auto",
          ); expand

    }; section

    object item
        extends Element("item")(
          clickable,
          display.inlineFlex,
          justifyContent.center,
          alignItems.center,
          height := 100.pct,
          padding := "0 1rem",
          PseudoClass.Hover(
            backgroundColor := colorPrimaryAccent,
          ),
        ); item

  }; navBar

  object pageMessages extends Block("page-messages")() {

    object message
        extends Element("message")(
          clickable,
          padding := "2px 5px",
          border := "1px solid black",
          whiteSpace.pre,
        ); message

  }; pageMessages

  object formField
      extends Block("form-field")(
        padding := 0.5.rem,
      ) {

    object label
        extends Element("label")(
          marginLeft := 0.5.rem,
          display.block,
          fontWeight := "500",
          fontSize := 1.25.rem,
        ); label

    object input
        extends Element("input")(
          display.block,
          padding := "0.2rem 0.5rem",
          borderRadius := 0.75.rem,
          border := "3px solid #333", // TODO (KR) :
          outline.unset,
          backgroundColor := "#f5f5f5", // TODO (KR) :
          fontFamily := "monospace",
          fontWeight := "600",
          width := 30.ch,
        ); input

  }; formField

  object formSubmit
      extends Block("form-submit")(
        pb,
        borderRadius := 0.5.rem,
        marginLeft := 2.rem,
      ); formSubmit

  object stdTable
      extends Block("std-table")(
        border := "1px solid black",
        borderCollapse.collapse,
        inTag("th")(
          bgSecondaryLight,
        ),
        inTag("th", "td")(
          border := "1px solid black",
          padding := "2px 10px",
        ),
      ); stdTable

  object fileInput
      extends Block("file-input")(
        pb,
        display.inlineBlock,
        borderRadius := 0.75.rem,
        border := "3px solid #333",
        padding := "1em 3em",
        fontSize := 1.5.em,
        inTag("ul")(
          paddingLeft := 1.rem,
        ),
      ); fileInput

}

type DefaultStyleSheet = _DefaultStyleSheet
object DefaultStyleSheet extends _DefaultStyleSheet()(ColorPalate.default)
