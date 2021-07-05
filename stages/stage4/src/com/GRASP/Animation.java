package com.GRASP;

import java.lang.Math;

abstract class Animation {
    protected long progress_ms, duration_ms;

    AnimationSystem animationSystem;
    
    public final boolean is_running() {
	return animationSystem
	    .pending
	    .containsKey(this);
    }

    public static final float tween(float start,
				    float end,
				    float progress) {
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
	animationSystem.add(this);
	this.duration_ms = duration_ms;
    }

    public void stop() {
	animationSystem.remove(this);
    }

    protected abstract void advance(float progress);
    
    public final void step() {
	if (progress_ms > duration_ms) {
	    stop();
	    return;
	}
	progress_ms += AnimationSystem.period_ms;

	advance((float) progress_ms / (float) duration_ms);
    }
}
