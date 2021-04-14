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
