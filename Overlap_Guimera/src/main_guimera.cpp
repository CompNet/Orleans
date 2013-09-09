#include <stdlib.h>
#include <stdio.h>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <vector>
#include <map>

#include "../include/graph_binary.h"
#include "../include/graph.h"

/* Convertit un fichier a l'envers, puis compare au fichier existant */

int main(int argc, char** argv) {

	/* Extraction du fichier normal depuis le fichier binaire */
	Graph* guimera = NULL;
	guimera = new Graph(argv[1], NULL, UNWEIGHTED, false);
	//cout << "Computing overlap..." << endl;
	//(*guimera).computeOverlap();
	cout << "done." << endl;
	cout << "Computing Guimera..." << endl;
	(*guimera).readCommunity(argv[2]);
	(*guimera).writeGuimera(argv[3]);
	cout << "done" << endl;

//	delete guimera;

	return 0;

}
