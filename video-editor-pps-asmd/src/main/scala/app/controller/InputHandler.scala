package app.controller

import scalafx.scene.input.{KeyEvent, KeyCode}
class InputHandler(onTogglePlayback: () => Unit):

  def handleKeyEvent(event: KeyEvent): Unit =
    event.code match
      case KeyCode.Space =>
        onTogglePlayback()
        event.consume()

      case _ =>