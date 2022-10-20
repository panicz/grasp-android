(require 'android-defs)

(activity GRASP
  (on-create-view
    (define mTts
      (android.speech.tts.TextToSpeech
	(this)
	(lambda (i) ())))
    (android.widget.LinearLayout (this)
      orientation: android.widget.LinearLayout:VERTICAL
      view:
	(android.widget.TextView (this)
	  text: "Enter the text to speak")
      view:
	(android.widget.EditText (this)
	  id: 101)
      view:
	(android.widget.Button (this)
	  text: "Speak!"
	  on-click-listener:
	    (lambda (v)
	      (mTts:speak
		((as <android.widget.EditText>
		  ((this):findViewById 101)):getText)
		android.speech.tts.TextToSpeech:QUEUE_FLUSH
		#!null))))))
