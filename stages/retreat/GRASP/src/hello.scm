(import (sweetex))
(use-sweetex)

(require 'android-defs)


;;(use-sweetex)

(activity hello
  (on-create-view
   (android.widget.TextView
    (this)
    text: "Hello, Android from Kawa Scheme!")))
