package com.GRASP;
import android.graphics.Canvas;
import android.view.KeyEvent;

import android.graphics.Paint;
//import android.graphics.TextPaint;
import android.graphics.Color;
import android.graphics.Typeface;


//import android.graphics.Canvas;
//import android.view.View;


public class Flex extends MultiBox {
    // 1. mozliwosc rozciagania (prawy dolny rog)
    // 2. mozliwosc przesuwania  i umieszczania
    //  (lewy gorny rog)
    // 3. mozliwosc zmiany rodzaju pudelka
    // (przytrzymujemy i wybieramy dostepne
    // opcje)
    ActionResult self;
    GestureHandler reaction =
	new GestureHandler(impassive,
			   impassive,
			   impassive,
			   untouchable);

    TypeHandler onTypeKey = criticize;

    DrawingMethod visualization = null;

    Object data = null;    

    @Override
    public void draw(Canvas canvas) {
	super.draw(canvas);
	if (visualization != null) {
	    canvas.translate(area.left, area.top);
	    visualization.draw(this, canvas);
	    canvas.translate(-area.left, -area.top);
	}
    }
    
    public Flex(float l, float t, float r, float b) {
	super(l, t, r, b);
	self = new ActionResult(this);
    }
    
    float start_x, start_y;
    
    @Override
    public ActionResult onPress(float x, float y,
				int finger) {
	ActionResult result =
	    super.onPress(x, y, finger);

	if (result.status == ActionStatus.Ignored
	    && finger == 0) {
	    start_x = x;
	    start_y = y;
	    return self;
	}
	else if (result.status ==
		 ActionStatus.ReturnedBox) {
	    /*
	    if(result.box instanceof MultiBox) {
		MultiBox mbox = (MultiBox) result.box;
		mbox.area.right += area.left;
		mbox.area.left += area.left;
		mbox.area.bottom += area.top;
		mbox.area.top += area.top;
		}*/
	    if (children.contains(result.box)) {
		//GRASP.Log("removing "+result.box
		//	  +" from "+this);
		children.remove(result.box);
		if (input_receiver == result.box) {
		    input_receiver = null;
		}
	    }
	    return result;
	} else {
	    return result;
	}
    }

    public ActionResult onMotion(float [] x, float [] y,
				 boolean [] finger,
				 int max_fingers) {
	ActionResult result =
	    reaction.onMotion.action(x, y, finger,
				     max_fingers);
	if (result.status != ActionStatus.Ignored) {
	    return result;
	}
	
	if (max_fingers >= 0 && finger[0]) {
	    moveBy(x[0] - start_x,
		   y[0] - start_y);
	    start_x = x[0];
	    start_y = y[0];
	    return ActionProcess;
	}
	else {
	    return ActionIgnore;
	}
    }

    @Override
    public boolean accepts(Box b, float x, float y) {
	GRASP.Log(area+" vs "+x+", "+y);
	return b instanceof Flex
	    && super.contains(x, y);
    }

    

    /*
    class FlexToButton implements TouchHandler {
	public Flex target;
	public ListBox source;
	
	@Override
	public ActionResult action(float x, float y) {
	    // trzeba odpowiednio ustawic wartosci
	    // callbackow targetu

	    // pokaz ekran konfiguracji
	    target.reaction.onHold = ...;

	    // wywolaj customowa funkcje 
	    target.reaction.onSingleTap = ...;

	    // wywolaj customowa funkcje
	    target.reaction.onDoubleTap = ...;
	    
	    return ActionProcess;
	}
	}    */

    public FlexText flexText = new FlexText();
    
    Box specialize(Flex me, float x, float y) {
	Button text =
	    new Button("Text",
		       showKeyboard,
		       positive,
		       positive,
		       caressing);
	Button button =
	    new Button("Button",
		       new LogTouch("Button"),
		       positive,
		       positive,
		       caressing);
	Button code =
	    new Button("Code",
		       new LogTouch("Code"),
		       positive,
		       positive,
		       caressing);
	    
	ListBox options = new ListBox(x, y, text,
				  button, code);
	
	text.action.onSingleTap
	    = new FlexText.FlexToText(me, options);
	
	return options;
    }

    @Override
    public ActionResult onSingleTap(float x, float y) {
	ActionResult result =
	    reaction.onSingleTap.action(x, y);
	if (result.status == ActionStatus.Ignored) {
	    result = super.onSingleTap(x, y);
	}
	return result;
    }

    @Override
    public ActionResult onDoubleTap(float x, float y) {
	ActionResult result =
	    reaction.onDoubleTap.action(x, y);
	if (result.status == ActionStatus.Ignored) {
	    result = super.onDoubleTap(x, y);
	}
	return result;
    }

    @Override
    public ActionResult onHold(float x, float y) {
	ActionResult result =
	    reaction.onHold.action(x, y);
	if (result.status == ActionStatus.Ignored) {
	    result = super.onHold(x, y);
	}
	if (result.status == ActionStatus.Ignored) {
	    //GRASP.Log("hold creates options list");
	    return new ActionResult(specialize(this,
					       x, y));
	}
	else if (result.status ==
		 ActionStatus.ReturnedBox
		 && result.box instanceof MultiBox) {
	    MultiBox m = (MultiBox) result.box;
	    //GRASP.Log("hold passes "+m+" above");
	    m.moveBy(area.left, area.top);
	}
	return result;
    }

    @Override
    public ActionResult onKeyDown(KeyEvent event) {
	ActionResult result = super.onKeyDown(event);
	if(result.status == ActionStatus.Ignored) {
	    /*	    if (event.isPrintingKey()) {
		int u = event.getUnicodeChar();
		GRASP.Log(Character.toString((char)u));
	    }
	    else {
		GRASP
		    .Log(KeyEvent
			 .keyCodeToString(event
					  .getKeyCode()));
					  }*/
	    return onTypeKey.action(event);
	}
	return result;
    }

    @Override
    public ActionResult onKeyUp(KeyEvent event) {
	ActionResult result = super.onKeyUp(event);
	if(result.status == ActionStatus.Ignored) {

	    return ActionProcess;
	}
	return result;
    }

    /*
    @Override
    public void onDragIn(Box b, float x, float y) {
       
    }

    @Override
    public void onDragOut(Box b, float x, float y) {
	
    }*/

    @Override
    public ActionResult onDragOver(Box b,
				   float x, float y) {
	ActionResult result = super.onDragOver(b, x, y);
	if (result.status == ActionStatus.Ignored) {
	    if (b instanceof Flex) {
		Flex box = (Flex) b;
		
	    }
	    return self;
	}
	else if (result.status ==
		 ActionStatus.ReturnedBox) {
	    if (result.box instanceof Flex) {
		Flex child = (Flex) result.box;
		// powinnismy dostosowac rozmiar
	    }
	    return self;
	}
	else {
	    return self;
	}
    }
    
    @Override
    public boolean is_embeddable() {
	return true;
    }

}
