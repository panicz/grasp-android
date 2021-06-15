package com.GRASP;


import android.os.Handler;
import android.view.View;

import java.util.Timer;
import java.util.TimerTask;

import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentHashMap;

import java.util.Iterator;

class AnimationSystem {
    View screen;
    Timer animate;
    TimerTask nextFrame;
    boolean running = false;
    Handler sync = new Handler();
    ConcurrentMap<Animation, Boolean> pending =
	new ConcurrentHashMap<Animation, Boolean>();
    public static final long period_ms = 40; // 25Hz

    public AnimationSystem(View view) {
	screen = view;
	//start();
    }

    void add(Animation animation) {
	pending.put(animation, true);
	if(!running) {
	    start();
	}
    }

    void remove(Animation animation) {
	pending.remove(animation, true);
	//if(running && pending.isEmpty()) {stop();}
    }
    
    void prepare() {
	animate = new Timer();
	nextFrame = new TimerTask() {
		public void run() {
		    sync.post(new Runnable() {
			    public void run() {
				Iterator<Animation> anim =
				    pending.keySet().iterator();
				
				while (anim.hasNext()) {
				    anim.next().step();
				}
				screen.invalidate();
			    }
			});
		}
	    };
    }

    void start() {
	GRASP.log("adding a new thread");
	animate.schedule(nextFrame, 0, period_ms);
	running = true;
    }

    void stop() {
	GRASP.log("stopping the thread");
	animate.cancel();
	animate.purge();
	running = false;
    }


}
