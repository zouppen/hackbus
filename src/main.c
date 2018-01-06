// -*- mode: c; c-file-style: "linux" -*-

#include <stdio.h>
#include <stdlib.h>
#include <modbus.h>
#include <errno.h>
#include <stdbool.h>
#include <unistd.h>

bool relay_control(modbus_t *mb, int slave, int relay, bool state);

bool relay_control(modbus_t *mb, int slave, int relay, bool state)
{
	int value = state ? 0x0100 : 0x0200;
	if (modbus_set_slave(mb, slave) == -1) return false;
	if (modbus_write_register(mb, relay, value) == -1) return false;
	return true;
}
	

int main(int argc, char **argv)
{
	modbus_t *mb;
	int ret = 0;

	mb = modbus_new_rtu(argv[1], 9600, 'N', 8, 1);
	if (mb == NULL) {
		fprintf(stderr, "Unable to create the libmodbus context\n");
		ret = -1;
		goto end;
	}

	if (modbus_connect(mb) == -1) {
		fprintf(stderr, "Unable to connect to bus\n");
		ret = -1;
		goto free;
	}
	
	bool state = false;
	while (true) {
		state = !state;
		for (int board = 1; board <= 2; board++) {
			for (int relay = 1; relay <= 4; relay++) {
				printf("%d %d %d\n", state, board, relay);
				bool ok = relay_control(mb, board, relay, state);
				if (!ok) {
					fprintf(stderr, "Control failed: %s\n", modbus_strerror(errno));
					ret = -1;
					goto free;
				}
				sleep(1);
			}
		}
	}
	
free:
	modbus_close(mb);
	modbus_free(mb);
end:
	return ret;
}
