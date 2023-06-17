package harness.webUI.style

import harness.webUI.vdom.{given, *}

object StyleSheetUtils {

  // =====| Colors |=====

  // --- Primary ---

  inline def colorPrimary(implicit colorPalate: ColorPalate): String = colorPalate.primary.color
  inline def colorPrimaryAccent(implicit colorPalate: ColorPalate): String = colorPalate.primary.accent
  inline def colorPrimaryLight(implicit colorPalate: ColorPalate): String = colorPalate.primary.light
  inline def colorPrimaryDark(implicit colorPalate: ColorPalate): String = colorPalate.primary.dark
  inline def colorPrimaryFont(implicit colorPalate: ColorPalate): String = colorPalate.primary.fontColor

  def bgPrimary(implicit colorPalate: ColorPalate): StyleElement =
    StyleElement(
      backgroundColor := colorPrimary,
      color := colorPrimaryFont,
    )

  def bgPrimaryAccent(implicit colorPalate: ColorPalate): StyleElement =
    StyleElement(
      backgroundColor := colorPrimaryAccent,
      color := colorPrimaryFont,
    )

  def bgPrimaryLight(implicit colorPalate: ColorPalate): StyleElement =
    StyleElement(
      backgroundColor := colorPrimaryLight,
      color := colorPrimaryFont,
    )

  def bgPrimaryDark(implicit colorPalate: ColorPalate): StyleElement =
    StyleElement(
      backgroundColor := colorPrimaryDark,
      color := colorPrimaryFont,
    )

  // --- Secondary ---

  inline def colorSecondary(implicit colorPalate: ColorPalate): String = colorPalate.secondary.color
  inline def colorSecondaryAccent(implicit colorPalate: ColorPalate): String = colorPalate.secondary.accent
  inline def colorSecondaryLight(implicit colorPalate: ColorPalate): String = colorPalate.secondary.light
  inline def colorSecondaryDark(implicit colorPalate: ColorPalate): String = colorPalate.secondary.dark
  inline def colorSecondaryFont(implicit colorPalate: ColorPalate): String = colorPalate.secondary.fontColor

  def bgSecondary(implicit colorPalate: ColorPalate): StyleElement =
    StyleElement(
      backgroundColor := colorSecondary,
      color := colorSecondaryFont,
    )

  def bgSecondaryAccent(implicit colorPalate: ColorPalate): StyleElement =
    StyleElement(
      backgroundColor := colorSecondaryAccent,
      color := colorSecondaryFont,
    )

  def bgSecondaryLight(implicit colorPalate: ColorPalate): StyleElement =
    StyleElement(
      backgroundColor := colorSecondaryLight,
      color := colorSecondaryFont,
    )

  def bgSecondaryDark(implicit colorPalate: ColorPalate): StyleElement =
    StyleElement(
      backgroundColor := colorSecondaryDark,
      color := colorSecondaryFont,
    )

  // --- Background ---

  inline def colorBackground(implicit colorPalate: ColorPalate): String = colorPalate.background.color
  inline def colorBackgroundFont(implicit colorPalate: ColorPalate): String = colorPalate.background.fontColor

  def bgBackground(implicit colorPalate: ColorPalate): StyleElement =
    StyleElement(
      backgroundColor := colorPalate.background.color,
      color := colorPalate.background.fontColor,
    )

  // --- Surface ---

  inline def colorSurface(implicit colorPalate: ColorPalate): String = colorPalate.background.color
  inline def colorSurfaceFont(implicit colorPalate: ColorPalate): String = colorPalate.background.fontColor

  def bgSurface(implicit colorPalate: ColorPalate): StyleElement =
    StyleElement(
      backgroundColor := colorPalate.surface.color,
      color := colorPalate.surface.fontColor,
    )

  // --- Error ---

  inline def colorError(implicit colorPalate: ColorPalate): String = colorPalate.background.color
  inline def colorErrorFont(implicit colorPalate: ColorPalate): String = colorPalate.background.fontColor

  def bgError(implicit colorPalate: ColorPalate): StyleElement =
    StyleElement(
      backgroundColor := colorPalate.error.color,
      color := colorPalate.error.fontColor,
    )

  // =====| Misc |=====

  val clickable: StyleElement =
    StyleElement(
      cursor.pointer,
      userSelect.none,
    )

  val buttonStyle: StyleElement =
    StyleElement(
      clickable,
      border := "2px solid black",
      padding := "5px 10px",
    )

  def pb(implicit colorPalate: ColorPalate): StyleElement =
    StyleElement(
      buttonStyle,
      bgPrimary,
      PseudoClass.Hover(
        backgroundColor := colorPrimaryAccent,
      ),
    )

}
