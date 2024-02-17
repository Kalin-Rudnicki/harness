package harness.webUI.style

final case class ColorPalate(
    primary: ColorPalate.Group1,
    secondary: ColorPalate.Group1,
    background: ColorPalate.Group2,
    surface: ColorPalate.Group2,
    error: ColorPalate.Group2,
)
object ColorPalate {

  final case class Group1(
      color: String,
      accent: String,
      light: String,
      dark: String,
      fontColor: String,
  )

  final case class Group2(
      color: String,
      fontColor: String,
  )

  object implicits {
    implicit val default: ColorPalate = ColorPalate.default
  }

  val default: ColorPalate =
    ColorPalate(
      primary = ColorPalate.Group1(
        color = "#20A9FE",
        accent = "#1d8ace",
        light = "#85D0FE",
        dark = "#003F66",
        fontColor = "#000000",
      ),
      secondary = ColorPalate.Group1(
        color = "#00B887",
        accent = "#05926d",
        light = "#35FD88",
        dark = "#028D3C",
        fontColor = "#000000",
      ),
      background = ColorPalate.Group2(
        color = "#2E3738",
        fontColor = "#E8F7EE",
      ),
      surface = ColorPalate.Group2(
        color = "#1C2122",
        fontColor = "#E8F7EE",
      ),
      error = ColorPalate.Group2(
        color = "#800E13",
        fontColor = "#000000",
      ),
    )

}
