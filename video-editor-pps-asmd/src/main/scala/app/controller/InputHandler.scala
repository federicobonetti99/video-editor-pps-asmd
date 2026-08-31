package app.controller

import scalafx.scene.input.{KeyEvent, KeyCode}

class InputHandler(
                    onTogglePlayback: () => Unit,
                    onUndo: () => Unit = () => (),
                    onRedo: () => Unit = () => ()
                  ):

  def handleKeyEvent(event: KeyEvent): Unit =
    val isCtrlOrMeta = event.controlDown || event.metaDown

    event.code match
      case KeyCode.Space =>
        onTogglePlayback()
        event.consume()

      case KeyCode.Z if isCtrlOrMeta && event.shiftDown =>
        onRedo()
        event.consume()

      case KeyCode.Z if isCtrlOrMeta =>
        onUndo()
        event.consume()

      case KeyCode.Y if isCtrlOrMeta =>
        onRedo()
        event.consume()

      case _ => ()