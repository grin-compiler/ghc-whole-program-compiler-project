#include <stddef.h>
#include <stdint.h>

unsigned int n_capabilities = 1;
int rts_isDynamic(void) {
  return 1;
}

int rts_isProfiled(void) {
  return 0;
}

void blockUserSignals(void) {
}

void unblockUserSignals(void) {
}

void startTimer(void) {
}

void stopTimer(void) {
}

void debugBelch() {
}

typedef struct _RTS_FLAGS {
} RTS_FLAGS;

RTS_FLAGS RtsFlags;

int keepCAFs;

void performGC(void) {
}

void performMajorGC(void) {
}

int getRTSStatsEnabled( void ) {
    return 0;//RtsFlags.GcFlags.giveStats != NO_GC_STATS;
}

uint32_t enabled_capabilities = 1;

int *stable_ptr_table = NULL;

void base_GHCziTopHandler_runIO_closure(){}
