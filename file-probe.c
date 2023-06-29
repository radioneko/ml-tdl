#define CAML_NAME_SPACE
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <X11/Xlib.h>
#include <X11/XKBlib.h>

#include <caml/mlvalues.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

CAMLprim value file_probe(value pathname, value expected_size)
{
	/*printf("path: %s, Expected_size: %ld\n", String_val(pathname), Long_val(expected_size));*/
	struct stat st;
	int fd = open(String_val(pathname), O_RDONLY);
	if (fd == -1)
		return Val_int(0);
	if (fstat(fd, &st) == -1 || !S_ISREG(st.st_mode) || st.st_size != Long_val(expected_size))
		goto return_false;
	off_t hole_off = lseek(fd, 0, SEEK_HOLE);
	if (hole_off == (off_t)-1)
		goto return_false;
	close(fd);
	return Val_int(hole_off == st.st_size);
return_false:
	close(fd);
	return Val_int(0);
}


#if 1
CAMLprim value query_mod_state()
{
	static Display *kdpy;

	if (kdpy == NULL && getenv("DISPLAY") != NULL) {
		int reason, code, err, major = XkbMajorVersion, minor = XkbMajorVersion;
		kdpy = XkbOpenDisplay("", &code, &err, &major, &minor, &reason);
	}

	if (kdpy != NULL) {
		XkbStateRec xs;
		memset(&xs, 0, sizeof(xs));
		XkbGetState(kdpy, XkbUseCoreKbd, &xs);
		return Val_int(xs.mods);
	}

	return Val_int(0);
}
#endif
