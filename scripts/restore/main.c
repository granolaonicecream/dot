#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>

void restoreWindow(Display* dpy, Window window) {
	XEvent xev;
	
	memset(&xev, 0, sizeof(xev));
	xev.xclient.type = ClientMessage;
	xev.xclient.serial = 0;
	xev.xclient.send_event = True;
	xev.xclient.message_type = XInternAtom(dpy, "WM_CHANGE_STATE", False);
	xev.xclient.window = window;
	xev.xclient.format = 32;
	xev.xclient.data.l[0] = NormalState;
	
	XSendEvent(dpy, DefaultRootWindow(dpy), False, 
			SubstructureNotifyMask | SubstructureRedirectMask, &xev);
}

int main(int argc, char *argv[]) {
	Window window = strtoul(argv[1],NULL,10);
	Display *dpy;
	dpy = XOpenDisplay(NULL);
	if (dpy == NULL) {
		fprintf(stderr, "Can't open display\n");
		exit(1);
	}
	restoreWindow(dpy, window);
	XFlush(dpy);
	return 0;
}
