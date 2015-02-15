#include <HsFFI.h>

static void hjass_enter(void) __attribute__((constructor));
static void hjass_enter(void)
{
	static char *argv[] = { "libHShjass.so", 0 }, **argv_ = argv;
	static int argc = 1;
	hs_init(&argc, &argv_);
}

static void hjass_exit(void) __attribute__((destructor));
static void hjass_exit(void)
{
	hs_exit();
}
