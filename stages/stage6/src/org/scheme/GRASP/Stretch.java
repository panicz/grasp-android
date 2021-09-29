package org.scheme.GRASP;

class Stretch implements Drag {

    Editor target;
    byte finger;

    float dx = 0;
    float dy = 0;
	
    public Stretch(Editor target, byte finger,
		   float start_x, float start_y) {
	Panel.stretches++;
	this.finger = finger;
	this.target = target;
	byte index = target.occupy_first_free_index(finger);
	target.pending_x[index] = start_x;
	target.pending_y[index] = start_y;
	target.transform.anchor(target.pending_x,
				target.pending_y,
				target.pending);
    }

    @Override
    public void move(Screen screen, float x, float y,
		     float _dx, float _dy) {
	byte index = target.pending_index[finger];
	target.pending_x[index] = x + dx;
	target.pending_y[index] = y + dy;
    }

    @Override
    public void drop(Screen screen, float x, float y,
		     float vx, float vy) {
	target.release_index(finger);
	Panel.stretches--;
	target.transform.anchor(target.pending_x,
				target.pending_y,
				target.pending);
    }

    @Override
    public Drag outwards(Transform transform) {
	float x = transform.unx(dx, dy);
	float y = transform.uny(dx, dy);
	dx = x;
	dy = y;
	return this;
    }

    @Override
    public Drag inwards(Transform transform) {
	float x = transform.x(dx, dy);
	float y = transform.y(dx, dy);
	dx = x;
	dy = y;

	return this;
    }
}
