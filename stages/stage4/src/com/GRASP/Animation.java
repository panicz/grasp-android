package com.GRASP;

import java.lang.Math;

public class Animation {

    Editor editor;

    float source_angle, target_angle;
    float source_scale, target_scale;

    float fixed_x, fixed_y;
    float source_x, source_y;
    float target_x, target_y;

    
    long progress_ms, duration_ms;

    boolean preserve_fixed = false;

    
    public Animation(Editor target) {
	editor = target;
    }
    
    public boolean is_running() {
	return editor
	    .screen
	    .animationSystem
	    .pending
	    .containsKey(this);
    }

    static float tween(float start, float end, float progress) {
	if (progress <= 0) {
	    return start;
	}
	if (progress >= 1) {
	    return end;
	}
	return (float)
	    (start + (end-start)*Math.sin(progress*Math.PI/2.0)); 
    }
    
    public void start(int duration_ms) {
	assert(duration_ms > 0);
	progress_ms = 0;
	editor.screen.animationSystem.add(this);
	this.duration_ms = duration_ms;
    }
    
    public void stop() {
	editor.screen.animationSystem.remove(this);
    }

    public void step() {
	if (progress_ms > duration_ms) {
	    stop();
	    return;
	}
	
	progress_ms += AnimationSystem.period_ms;
	float progress = (float) progress_ms / (float) duration_ms;

	if (preserve_fixed) {
	    float left = editor.transform.getLeft();
	    float top = editor.transform.getTop();
	
	    float x0 = editor.transform.unx(fixed_x, fixed_y);
	    float y0 = editor.transform.uny(fixed_x, fixed_y);
	
	    editor.transform.setScale(tween(source_scale,
					    target_scale,
					    progress));
	
	    editor.transform.setAngle(tween(source_angle,
					    target_angle,
					    progress));
	    float x1 = editor.transform.unx(fixed_x, fixed_y);
	    float y1 = editor.transform.uny(fixed_x, fixed_y);

	    editor.transform.setLeft(left + (x1-x0));
	    editor.transform.setTop(top + (y1-y0));	
	}
	else {
	    editor.transform.setScale(tween(source_scale,
					    target_scale,
					    progress));
	
	    editor.transform.setAngle(tween(source_angle,
					    target_angle,
					    progress));
	    editor.transform.setLeft(tween(source_x,
					   target_x,
					   progress));
	
	    editor.transform.setTop(tween(source_y,
					  target_y,
					  progress));
	}
	
    }

    public void setTargetAngle(float angle_deg) {
	source_angle = editor.transform.getAngle();
	target_angle = angle_deg;
    }

    public void fixPoint(float x, float y) {
	fixed_x = x;
	fixed_y = y;
	preserve_fixed = true;
    }

    public void setScroll(float x, float y) {
	source_x = editor.transform.getLeft();
	source_y = editor.transform.getTop();
	target_x = x;
	target_y = y;
	preserve_fixed = false;
    }
    

    public void setTargetScale(float scale) {
	source_scale = editor.transform.getScale();
	target_scale = scale;
    }

    public void setTargetTransform(float left, float top,
				   float scale, float angle) {

    }
    

}
