#include <stdlib.h>
#include <sys/types.h>
#include <stdio.h>

/*
	C version of the navigator program.
	Parse the input, load it in proper structures and runs the floyd warshall algorithm.
	Everything can stay in ram.
*/

int main (int argc, char const *argv[])
{
	if (argc > 3) {
		usage();
	}
	
	if (argc == 1) {
		char map = "stradario.txt";
		char path = "percorso.txt";
	}
	for (i = 0; i < argc; i++) {
		
	}
	
	char **parsed = parse();
	return 0;
}

void usage() {
	printf "./nav [map] [path]";
}
