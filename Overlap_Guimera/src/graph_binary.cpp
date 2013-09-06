#include <sys/mman.h>
#include <fstream>
#include<sstream>
#include <string>
#include <stack>
#include <set>
#include<string.h>
#include "../include/graph_binary.h"
#include "math.h"
#include <omp.h>
#include <limits>
#include <algorithm>

using namespace std;

Graph::Graph() {
  nb_nodes     = 0;
  nb_links_out     = 0;
  nb_links_in     = 0;
  total_weight = 0;
  total_weight_in = 0;
  total_weight_out = 0;
}

Graph::Graph(char *filename, char *filename_w, int type, bool renumbered) {
cout << "ouverture " << endl;
  ifstream finput;
  finput.open(filename,fstream::in | fstream::binary);
  
  // Read number of nodes on 4 bytes
  finput.read((char *)&nb_nodes, sizeof(int));
  assert(finput.rdstate() == ios::goodbit);

  // Read cumulative out degree sequence: 8 bytes for each node
  // cum_degree[0]=degree(0); cum_degree[1]=degree(0)+degree(1), etc.
  degrees_out.resize(nb_nodes);
  finput.read((char *)&degrees_out[0], nb_nodes*sizeof(long));
  
  // Read cumulative in degree sequence: 8 bytes for each node
  // cum_degree[0]=degree(0); cum_degree[1]=degree(0)+degree(1), etc.
  degrees_in.resize(nb_nodes);
  finput.read((char *)&degrees_in[0], nb_nodes*sizeof(long));
  cout << "degrees read " << endl;

  if (renumbered) {
  // Read correspondance of labels
  correspondance.resize(nb_nodes);
  finput.read((char *)(&correspondance[0]), nb_nodes*sizeof(int)); 

  
		  cout << "corresp read "<< endl;
    }
  // Read links_out: 4 bytes for each link 
  nb_links_out=degrees_out[nb_nodes-1];
  links.resize(nb_links_out);
  finput.read((char *)(&links[0]), nb_links_out*sizeof(int)); 
 
  // Read links_in: 4 bytes for each link 
  nb_links_in=degrees_in[nb_nodes-1];
  links_in.resize(nb_links_in);
  finput.read((char *)(&links_in[0]), nb_links_in*sizeof(int)); 
  
 cout << "links read " << endl;
  // IF WEIGHTED : read weights: 4 bytes for each link (each link is counted twice)
  weights.resize(0);
  total_weight=0;
  if (type==WEIGHTED) {
    ifstream finput_w;
    finput_w.open(filename_w,fstream::in | fstream::binary);
    weights.resize(nb_links_out);
    finput_w.read((char *)&weights[0], nb_links_out*sizeof(int));  
  }    

   
  // Compute total weight
  for (unsigned int i=0 ; i<nb_nodes ; i++) {
    total_weight_in = (double)in_weighted_degree(i);
    total_weight_out = (double)out_weighted_degree(i);
    total_weight += (double)weighted_degree(i);
  }
  cout << "Fin de lecture " << endl;
}

Graph::Graph(int n, int m, double t, int *d, int *l, float *w) {
/*  nb_nodes     = n;
  nb_links     = m;
  total_weight = t;
  degrees_out      = d;
  links        = l;
  weights      = w;*/
}

void
Graph::display() {
/*  for (unsigned int node=0 ; node<nb_nodes ; node++) {
    pair<vector<unsigned int>::iterator, vector<float>::iterator > p = neighbors(node);
    for (unsigned int i=0 ; i<nb_neighbors_out(node) ; i++) {
      if (node<=*(p.first+i)) {
	if (weights.size()!=0)
	  cout << node << " " << *(p.first+i) << " " << *(p.second+i) << endl;
	else
	  cout << node << " " << *(p.first+i) << endl;
      }
    }   
  }*/
  for (unsigned int node=0 ; node<nb_nodes ; node++) {
    pair<vector<unsigned int>::iterator, vector<float>::iterator > p = neighbors(node);
    cout << correspondance[node] << ":" ;
    for (unsigned int i=0 ; i<nb_neighbors_out(node) ; i++) {
      if (true) {
	if (weights.size()!=0)
	  cout << " (" << correspondance[*(p.first+i)] << " " << *(p.second+i) << ")";
	else
	  cout << " " << correspondance[*(p.first+i)];
      }
    }
    cout << endl;
  }
}

/* Methode de reecriture du fichier */ 
void
Graph::writeFile(string outNeighbors, string inNeighbors) {

	ofstream foutput;
	foutput.open(outNeighbors.c_str() ,fstream::out | fstream::binary);
	
	cout << "Nombre de noeuds : " << nb_nodes << endl; 

	/* On recupere les voisins sortants */ 
	for(unsigned int node=0; node < nb_nodes; node++) {
	
		pair<vector<unsigned int>::iterator, vector<float>::iterator > p = neighbors(node);
	
		for(unsigned int i = 0; i < nb_neighbors_out(node); i++) {
			
			foutput << correspondance[node] << " " << correspondance[*(p.first+i)] << endl;
			
		}

	}

	foutput.close();

	ofstream foutputIn;
        foutputIn.open(inNeighbors.c_str(), fstream::out | fstream::binary);

	/* On recupere les voisins entrants */
	for(unsigned int node=0; node < nb_nodes; node++) {
	
		pair<vector<unsigned int>::iterator, vector<float>::iterator > p1 = in_neighbors(node);

		for(unsigned int i = 0; i < nb_neighbors_in(node); i++) {

				foutputIn << correspondance[node] << " " << correspondance[*(p1.first+i)] << endl;
		
		}

	}

}

/************************************************************************

 ************************************************************************
 *
 *
 *
 *
 * OVERLAP AND RATIO PROCESSING
 * 
 *
 *
 *
 ***********************************************************************
 ***********************************************************************
*/

void 
Graph::computeOverlap(string fileName) {

	ofstream foutput; 
	foutput.open(fileName.c_str(), fstream::out | fstream::binary);
	foutput.precision(15);

	unsigned int deg;
	//unsigned int* outNeighbors;
	//unsigned int* inNeighbors;
	float overlap = 0;
	double ratio = 0.000000;
	double in, out;
	for(unsigned int node = 0; node < nb_nodes; node++) {
	
		if(node == 0) deg = 0; else deg = node - 1;

		unsigned int* outNeighbors;
		unsigned int* inNeighbors;
if(node == 0) {
		outNeighbors = &links[0];
		inNeighbors = &links_in[0];
} else {
		outNeighbors = &links[degrees_out[deg]];
		inNeighbors = &links_in[degrees_in[deg]];

}/*
		unsigned int* tmp = new unsigned int[nb_neighbors_out(node)];
		unsigned int* tmp1 = new unsigned int [nb_neighbors_in(node)];
	
		for(int i = 0; i < nb_neighbors_out(node); i++) 
			tmp[i] = outNeighbors[i];

		for(int i = 0; i < nb_neighbors_in(node); i++)
			tmp1[i] = inNeighbors[i];	
*/				
		vector<int>* inter;
		inter = new vector<int>(max(nb_neighbors_out(node), nb_neighbors_in(node)));
		vector<int>::iterator it;

		sort(outNeighbors, outNeighbors + nb_neighbors_out(node));
		sort(inNeighbors, inNeighbors + nb_neighbors_in(node));

		it = set_intersection(outNeighbors, outNeighbors + nb_neighbors_out(node), inNeighbors, inNeighbors + nb_neighbors_in(node), (*inter).begin());
		(*inter).resize(it-(*inter).begin());
		if(nb_neighbors_out(node) == 0 || nb_neighbors_in(node) == 0) {	
			ratio = 0;
			overlap = 0;
		}
		else { 
			out = nb_neighbors_out(node);
			in = nb_neighbors_in(node);	
			ratio = out / in;
			overlap = max((*inter).size()/(float)nb_neighbors_out(node), (*inter).size()/(float)nb_neighbors_in(node)); 
		}
		foutput << correspondance[node] << ";" << nb_neighbors_out(node) << ";" << nb_neighbors_in(node) << ";" << ratio << ";" <<  overlap << endl;  
//		foutput << node << ";" << nb_neighbors_out(node) << ";" << nb_neighbors_in(node) << ";" << ratio << ";" <<  overlap << endl;
		//delete[] outNeighbors; 
		//delete[] inNeighbors; 
		//delete inter;

	}

	foutput.close();

}


void
Graph::display_binary(char *outfile) {
  ofstream foutput;
  foutput.open(outfile ,fstream::out | fstream::binary);

  foutput.write((char *)(&nb_nodes),4);
  foutput.write((char *)(&degrees_out[0]),4*nb_nodes);
  foutput.write((char *)(&links[0]),4*nb_links_out);
  foutput.write((char *)(&degrees_in[0]),4*nb_nodes);
  foutput.write((char *)(&links_in[0]),4*nb_links_in);
}


/************************************************************************
 ************************************************************************
 *
 *
 *
 *
 *GUIMERA METHOD IMPROVED
 * 
 *
 *
 *
 ***********************************************************************
 ***********************************************************************
*/


	void
	Graph::readCommunity(string filename) {

		ifstream finput;
		unsigned int node, node_corr, com, corr;

		finput.open(filename.c_str(), fstream::in);
		node = 0; com = 0; corr = 0; 

		map_communities.resize(nb_nodes);
		int cpt = 0;


		cpt = 0;

		while (!finput.eof() && cpt < nb_nodes) {

			/* Strange: reads one line more than expected... */		
			cpt++; 
			if(finput.eof()) 

				break; 

			/* Getting a node and its community */
		        finput >> node_corr >> com;
			//node_corr--;
			/* Is it really needed? Depends on the community algorithm? */		
			//com = com - 1;

			/* We associate the community to the node */
		        map_communities[node_corr] = com; 
	
			if(communities.size() <= com) {

				communities.resize(com+1);
			}
	
			communities[com].push_back(node_corr); 		

		}

		finput.close();
	}

	float
	Graph::avg_degree_in_comm(int comm) {

		long degree_comm_global = 0;

		for(unsigned int i = 0; i < communities[comm].size(); i++) {

			degree_comm_global += degree_in_com[communities[comm][i]];//degree_in_comm(communities[comm][i], comm);

		}

		return  degree_comm_global / communities[comm].size();
	}

	float
	Graph::avg_degree_out_comm(int comm) {

		long degree_comm_global = 0;

		for(unsigned int i = 0; i < communities[comm].size(); i++) {

		        degree_comm_global += degree_out_com[communities[comm][i]];

		}

		return  degree_comm_global / communities[comm].size();
	}


	float
	Graph::degree_out_comm(int node, int comm) {

		unsigned int pos_out, pos_out_end;
		float cpt = 0.;
		 
		if(node == 0) pos_out = 0; else pos_out = degrees_out[node - 1];
		pos_out_end=degrees_out[node];

		for (unsigned int i=pos_out; i < pos_out_end; i++) {
			if (map_communities[links[i]] == comm)
				cpt++;
		}    
		return cpt;

	}

	float
	Graph::degree_in_comm(int node, int comm) {


		unsigned int pos_out, pos_out_end;
		float cpt = 0;
		 
		if(node == 0) pos_out = 0; else pos_out = degrees_in[node - 1];
		pos_out_end=degrees_in[node];
		    

		for (unsigned int i=pos_out; i < pos_out_end; i++) {
			if (map_communities[links_in[i]] == comm)
				cpt++;
		}    
		return cpt;

	}

	double
	Graph::variane_out(int comm) {

		double mean = avg_com_out[comm];
		/* TODO: do we calculate it here? Or before? */

		double tmp = 0;
		double degree_node = 0;

		for(unsigned int i = 0; i < communities[comm].size(); i++) {
	
			degree_node = degree_out_com[communities[comm][i]];
			tmp += (degree_node - mean) * (degree_node - mean);

		}

		return tmp / (double)communities[comm].size();

	}

	double
	Graph::variane_in(int comm) {

		double mean = avg_com_in[comm];
		/* TODO: do we calculate it here? Or before? */

		double tmp = 0;
		double tmp_bis = 0;
		double degree_node = 0;

		for(unsigned int i = 0; i < communities[comm].size(); i++) {

			degree_node = degree_in_com[communities[comm][i]];
			tmp_bis = degree_node - mean;
		        tmp += tmp_bis * tmp_bis;

		}

		return tmp / (double)communities[comm].size();

	}

	double 
	Graph::standard_deviation_out(int comm) {

		return sqrt(variane_out(comm));

	}

	double
	Graph::standard_deviation_in(int comm) {

		return sqrt(variane_in(comm));

	}

	double
	Graph::z_score_out(int node, int comm) {

		if(std_com_out[comm] == 0)

			return std::numeric_limits<double>::max();

		else

			return ((degree_out_com[node] - avg_com_out[comm]) / std_com_out[comm]);

	}

	double
	Graph::z_score_in(int node, int comm) {

		if(std_com_in[comm] == 0) 

			return numeric_limits<double>::max();

		else
	
			return ((degree_in_com[node] - avg_com_in[comm]) /  std_com_in[comm]);

	}


/*double
        Graph::participation_out(int node) {

                double sum = 0;
                double tmp = 0;

                for (unsigned int i = 0; i < communities.size(); i++) {
                
                        if(nb_neighbors_out(node) > 0) {

                                tmp = ((degree_out_comm(node, i)) / nb_neighbors_out(node)); 
                                sum += tmp * tmp;

                        }

                }

                return (1 - sum);

        }

double
        Graph::participation_in(int node) {

                double sum = 0;
                double tmp = 0;

                for (unsigned int i = 0; i < communities.size(); i++) {
                
                        if(nb_neighbors_in(node) > 0) {

                                tmp = ((degree_in_comm(node, i)) / nb_neighbors_in(node)); 
                                sum += tmp * tmp;

                        }

                }

                return (1 - sum);

        }*/


double
Graph::participation_out(int node) {

        //Un vecteur de taille égale au nombre de communautés, rempli de 0
        vector<unsigned int> communities_count(communities.size(),0);

        unsigned int value, communaute;

	// Parcours du voisinage des noeuds
	unsigned int pos_out, pos_out_end;
        if(node == 0) pos_out = 0; else pos_out = degrees_out[node - 1];
        pos_out_end=degrees_out[node];

	// A la fin, le vecteur contient le degre du noeud vers chaque communaute
        for (unsigned int i = pos_out; i < pos_out_end; i++) {
		// Communaute du noeud
                communaute = map_communities[links[i]];
                communities_count[communaute]++;
	}

	double sum = 0.;

	// Parcours du vecteur de communautes pour calculer la participation
	for(unsigned int i = 0; i < communities.size(); i++) {

		// Si le noeud a des voisins dans la communaute
		if(communities_count[i] > 0) {

			sum += ((double)(communities_count[i]) / (double)nb_neighbors_out(node)) * ((double)(communities_count[i]) / (double)nb_neighbors_out(node));

		}
	}

	return (1 - sum);

}

	double
Graph::participation_in(int node) {

        //Un vecteur de taille égale au nombre de communautés, rempli de 0
        vector<unsigned int> communities_count(communities.size(),0);

        unsigned int value, communaute;

        // Parcours du voisinage des noeuds
	unsigned int pos_in, pos_in_end;
        if(node == 0) pos_in = 0; else pos_in = degrees_in[node - 1];
        pos_in_end=degrees_in[node];

        // A la fin, le vecteur contient le degre du noeud vers chaque communaute
        for (unsigned int i = pos_in; i < pos_in_end; i++) {
                // Communaute du noeud
                communaute = map_communities[links_in[i]];
                communities_count[communaute]++;
        }

        double sum = 0.;

        // Parcours du vecteur de communautes pour calculer la participation
        for(unsigned int i = 0; i < communities.size(); i++) {

                // Si le noeud a des voisins dans la communaute
                if(communities_count[i] > 0) {

                        sum += ((double)(communities_count[i]) / (double)nb_neighbors_in(node)) * ((double)(communities_count[i]) / (double)nb_neighbors_in(node));

                }
        }

        return (1 - sum);

}

	unsigned int
	Graph::diversite(int node, bool out) {
		int com_node = map_communities[node];
		set<unsigned int> comm_ext;
		int com;
		if (out) {
			int pos_out, pos_out_end = degrees_out[node];
			if(node == 0) pos_out = 0; else pos_out = degrees_out[node - 1];
			//cout << "degre : " << pos_out_end - pos_out << endl;
			for (int i = pos_out; i < pos_out_end; i++) {
			//	cout << "BIIIIIIIM "<< endl;
				com = map_communities[links[i]];
			//	cout << links[i] << " dans communaute " << com << endl;
				if (com != com_node)
					comm_ext.insert(com);
			}
			//cout << "Diversite : " << comm_ext.size() << endl;
		}
		else {
			int pos_in, pos_in_end = degrees_in[node];
			if(node == 0) pos_in = 0; else pos_in = degrees_in[node - 1];
			for (int i = pos_in; i < pos_in_end; i++) {
				com = map_communities[links_in[i]];
				if (com != com_node)
					comm_ext.insert(com);
			}
		}
		
		return comm_ext.size();
	}

	float
	Graph::avg_diversite(int comm, bool out) {

		long diversite_comm_global = 0;
		for(unsigned int i = 0; i < communities[comm].size(); i++) {
			diversite_comm_global += diversite(communities[comm][i], out);
		}
		return  (float)diversite_comm_global / (float)communities[comm].size();
	}

	float
	Graph::variance_diversite(int comm, bool out) {
		//float mean = avg_diversite(comm, out);
		float mean;
		if (out)
			mean = avg_com_out[comm];
		else
			mean = avg_com_in[comm];
		/* TODO: do we calculate it here? Or before? */

		float tmp = 0;
		float diversite_node = 0;
		
		for(unsigned int i = 0; i < communities[comm].size(); i++) {
	
			diversite_node = diversite(communities[comm][i], out);
			tmp += (diversite_node - mean) * (diversite_node - mean);

		}

		return tmp / (float)communities[comm].size();
	}

	float
	Graph::standard_deviation_diversite(int comm, bool out) {

		return sqrt(variance_diversite(comm, out));

	}

	float
	Graph::z_diversite(int node, bool out) {
		int com_node = map_communities[node];
		float std;
		if (out)
			std = std_com_out[com_node];
		else 
			std = std_com_in[com_node];
		
		if(std == 0) 
			return numeric_limits<double>::max();
		else {
			if (out)
				return (((float)diversite(node, out) - avg_com_out[com_node]) / std_com_out[com_node]);
			else
				return (((float)diversite(node, out) - avg_com_in[com_node]) / std_com_in[com_node]);
		}	
			
			//return (((float)diversite(node, out) - avg_diversite(com_node, out)) / std);
		
	}


	unsigned int
	Graph::intensite(int node, bool out) {
		int com_node = map_communities[node];
		set<unsigned int> comm_ext;
		int cpt = 0;
		
		if (out) {
			int pos_out, pos_out_end = degrees_out[node];
			if(node == 0) pos_out = 0; else pos_out = degrees_out[node - 1];
			for (int i = pos_out; i < pos_out_end; i++) {
				if (map_communities[links[i]] != com_node)
					cpt++;
			}
		}
		else {
			int pos_in, pos_in_end = degrees_in[node];
			if(node == 0) pos_in = 0; else pos_in = degrees_in[node - 1];
			for (int i = pos_in; i < pos_in_end; i++) {
				if (map_communities[links_in[i]] != com_node)
					cpt++;
			}
		}
		return cpt;
	}

	float
	Graph::avg_intensite(int comm, bool out) {
		//cout << "calcul moyenne " << endl;
		long intensite_comm_global = 0;
		//cout << "com : " << comm << "size : " << communities[comm].size() <<endl;
		for(unsigned int i = 0; i < communities[comm].size(); i++) {
		//	cout << "intensite(" << communities[comm][i] <<", " << out<< ")";
			intensite_comm_global += intensite(communities[comm][i], out);
		//	cout << "ientesite " << intensite_comm_global << endl;
		}
		//cout << "taille " << communities[comm].size() << endl;
		return  (float)intensite_comm_global / (float)communities[comm].size();
	}

	float
	Graph::variance_intensite(int comm, bool out) {
		//cout << "laaaaa" << endl;
		float mean;
		if (out)
			mean = avg_com_out[comm];
		else
			mean = avg_com_in[comm];
		/* TODO: do we calculate it here? Or before? */
		//cout << "mean : " << mean << endl;
		float tmp = 0;
		float intensite_node = 0;
		//cout << "Ici" << endl;
		for(unsigned int i = 0; i < communities[comm].size(); i++) {
	
			intensite_node = intensite(communities[comm][i], out);
		//	cout << "Noeud " << communities[comm][i] <<" -> Intensite : "<< intensite_node << endl;
			tmp += (intensite_node - mean) * (intensite_node - mean);

		}

		return tmp / (float)communities[comm].size();
	}

	float
	Graph::standard_deviation_intensite(int comm, bool out) {
		//cout << variance_intensite(comm, out) << endl;
		return sqrt(variance_intensite(comm, out));

	}

	float
	Graph::z_intensite(int node, bool out) {
		int com_node = map_communities[node];
		float std;
		if (out)
			std = std_com_out[com_node];
		else 
			std = std_com_in[com_node];
		
		if(std == 0) 
			return numeric_limits<double>::max();
		else {
			if (out)
				return (((float)intensite(node, out) - avg_com_out[com_node]) / std_com_out[com_node]);
			else
				return (((float)intensite(node, out) - avg_com_in[com_node]) / std_com_in[com_node]);
		}	
		
	}

	float
	Graph::homogeneite(int node, bool out) {
		int com_node = map_communities[node];
		//Un vecteur de taille égale au nombre de communautés, rempli de 0
		vector<unsigned int> communities_count(communities.size(),0);
		unsigned int value, communaute;
		unsigned int nb_com= 0, cpt = 0;
		if (out) {
			unsigned int pos_out, pos_out_end = degrees_out[node];
			if(node == 0) pos_out = 0; else pos_out = degrees_out[node - 1];
			for (unsigned int i = pos_out; i < pos_out_end; i++) {
				communaute = map_communities[links[i]];
				//cout << links[i] << " dans communaute " << communaute << endl;
				if (communaute != com_node) {
					cpt++;
					if (communities_count[communaute] == 0)
						nb_com++;
					communities_count[communaute]++;
				}
			}
		}
		else {
			unsigned int pos_in, pos_in_end = degrees_in[node];
			if(node == 0) pos_in = 0; else pos_in = degrees_in[node - 1];
			for (unsigned int i = pos_in; i < pos_in_end; i++) {
				communaute = map_communities[links_in[i]];
				if (communaute != com_node) {
					cpt++;
					if (communities_count[communaute] == 0)
						nb_com++;
					communities_count[communaute]++;
				}
			}
		}
		//cout << "nb_com " << nb_com << endl;
		if (nb_com == 0)
			return 0;
		float mean = (float)cpt / (float)nb_com;
		float tmp = 0, tmp_bis = 0;


		for (unsigned int i = 0 ; i < communities.size(); i++) {
			if (communities_count[i] != 0) {
				tmp_bis = (communities_count[i] - mean);
				tmp +=  tmp_bis * tmp_bis;
			}
			
		}
		//cout << tmp << endl;
		return sqrt(tmp / (float)nb_com);
	}

	float
	Graph::avg_homogeneite(int comm, bool out) {

		float homogeneite_comm_global = 0;
		for(unsigned int i = 0; i < communities[comm].size(); i++) {
			homogeneite_comm_global += homogeneite(communities[comm][i], out);
		}
		return  (float)homogeneite_comm_global / (float)communities[comm].size();
	}

	float
	Graph::variance_homogeneite(int comm, bool out) {
		float mean;
		if (out)
			mean = avg_com_out[comm];
		else
			mean = avg_com_in[comm];
		/* TODO: do we calculate it here? Or before? */

		float tmp = 0;
		float homogeneite_node = 0;
		
		for(unsigned int i = 0; i < communities[comm].size(); i++) {
			
			homogeneite_node = homogeneite(communities[comm][i], out);
			//cout << "homogeneite : " <<homogeneite_node << endl;
			tmp += (homogeneite_node - mean) * (homogeneite_node - mean);

		}
		//cout << "tmp  : "<< tmp <<endl;
		//cout << "size  : "<< communities[comm].size()<<endl;
		return tmp / (float)communities[comm].size();
	}

	float
	Graph::standard_deviation_homogeneite(int comm, bool out) {
		//cout << variance_homogeneite(comm, out) <<endl;
		return sqrt(variance_homogeneite(comm, out));

	}
	
	float
	Graph::z_homogeneite(int node, bool out) {
		int com_node = map_communities[node];
		float std;
		if (out)
			std = std_com_out[com_node];
		else 
			std = std_com_in[com_node];
		cout << "std dev : " << std << endl;
		if (out)
			cout << "avg : " << avg_com_out[com_node] << endl;
		else
			cout << "avg : " << avg_com_in[com_node] << endl;
		if(std == 0) 
			return numeric_limits<double>::max();
		else	{
			if (out)
				return ((homogeneite(node, out) - avg_com_out[com_node]) / std_com_out[com_node]);
			else
				return ((homogeneite(node, out) - avg_com_in[com_node]) / std_com_in[com_node]);
		}	
		
	}


	void
	Graph::writeGuimera(string filename) {

		ofstream foutput;
		

		//foutput << "node;z_score_out;z_score_in;community;k_out;k_in;variane_out;variane_in;d_out;d_in;div_out;div_in;int_out;int_in;hom_out;hom_in" << endl;
		//foutput << "node;community;z_score_out;z_score_in;z_diversite_out;z_diversite_in;z_intensite_out;z_intensite_in;z_homogeneite_out;z_homogeneite_in;"<< endl;			
		
		avg_com_out.resize(communities.size());
		std_com_out.resize(communities.size());
		avg_com_in.resize(communities.size());
		std_com_in.resize(communities.size());
		degree_in_com.resize(nb_nodes);
		degree_out_com.resize(nb_nodes);
		
		
		
		
		unsigned int i, thread_id;

		for (i=0; i < nb_nodes; i++) {
			degree_in_com[i]= degree_in_comm(i, map_communities[i]);
			degree_out_com[i]=degree_out_comm(i,map_communities[i]);
		}

		//Z_score
		cout << "Calcul des degrés internes au communauté " <<endl;
		#pragma omp parallel private(thread_id)
		{ 
			thread_id = omp_get_thread_num();
			int cpt = 1;
			/*stringstream ss;
			ss << thread_id;
			string filename2 = filename+"_z_score_" + ss.str();*/
			#pragma omp for
			for (i=0; i < nb_nodes; i++) {
				degree_in_com[i]= degree_in_comm(i, map_communities[i]);
				degree_out_com[i]=degree_out_comm(i,map_communities[i]);
				if (cpt % 500 == 0)
					cout << thread_id <<" a traité 500 communautés" << endl; 
			}
			#pragma omp barrier
			#pragma omp master
			{
				cout << "Calcul des moyennes et écart-type pour le z-score " <<endl;
			}
			cpt=1;
			#pragma omp for
			for(i = 0; i < communities.size(); i++) {
				//cout << i << " effectué par " << omp_get_thread_num() << endl;
				avg_com_out[i]= avg_degree_out_comm(i);
				std_com_out[i]= standard_deviation_out(i);
				avg_com_in[i] = avg_degree_in_comm(i);
				std_com_in[i] = standard_deviation_in(i);
				if (cpt % 500 == 0)
					cout << thread_id <<" a traité 500 communautés" << endl; 
				cpt++;
			}
			
		}
		

		foutput.open((filename+"_z_score").c_str(), fstream::out);
		cout << "Calcul du z-score " <<endl;
		for(i = 0; i < nb_nodes; i++) {
			foutput << i <<";" << map_communities[i] << ";";
			foutput << z_score_out(i, map_communities[i]) << ";" << z_score_in(i, map_communities[i]) << ";" << endl;			
			if (i % 1000000 == 0)
				cout << "1 million de noeuds en plus pour le z_score : présence !" << endl;

		}
	
		foutput.close();
		
		degree_in_com.clear();
		degree_out_com.clear();
		
		//D'abord la diversité
		cout << "Calcul des moyennes et ecart type pour la diversité " <<endl;
		#pragma omp parallel private(thread_id, foutput) 
 		{
			thread_id = omp_get_thread_num();
			stringstream ss;
			ss << thread_id;
			string filename2 = filename+"_diversite_" + ss.str();
			int cpt = 1;
			#pragma omp for
			for(i = 0; i < communities.size(); i++) {
				avg_com_out[i]= avg_diversite(i, true);
				std_com_out[i]= standard_deviation_diversite(i, true);
				avg_com_in[i] = avg_diversite(i, false);
				std_com_in[i] = standard_deviation_diversite(i, false);	
				if (cpt % 500 == 0)
					cout << thread_id <<" a traité 500 communautés" << endl; 
				cpt++;
			}
			cpt=1;
			#pragma omp barrier
			#pragma omp master 
			{
				cout << "Calcul de la diversité " <<endl;
			}
			
			//cout << filename2 << endl;
			foutput.open((filename2).c_str(), fstream::out);
			#pragma omp for
			for(i = 0; i < nb_nodes; i++) {
				foutput << i <<";" << map_communities[i] << ";";
				foutput << z_diversite(i, true) << ";" << z_diversite(i, false) << ";" << endl;
				if (cpt % 200000 == 0)
					cout << thread_id <<" a traité 200000 noeuds" << endl; 
				cpt++;	
			}
			foutput.close();
			cpt=1;
			#pragma omp barrier
			#pragma omp master 
			{
				//Maintenant, l'intensité
					cout << "Calcul des moyennes et ecart type pour l'intensité " <<endl;
			}
			#pragma omp for
			for(i = 0; i < communities.size(); i++) {
				avg_com_out[i]= avg_intensite(i, true);
				std_com_out[i]= standard_deviation_intensite(i, true);
				avg_com_in[i] = avg_intensite(i, false);
				std_com_in[i] = standard_deviation_intensite(i, false);
				if (cpt % 500 == 0)
					cout << thread_id <<" a traité 500 communautés" << endl; 
				cpt++;
			}
			cpt=1;
			#pragma omp barrier
			#pragma omp master 
			{
				//Maintenant, l'intensité
					cout << "Calcul de l'intensité " <<endl;
			}
			filename2 = filename+"_intensite_" + ss.str();
			foutput.open((filename2).c_str(), fstream::out);
			#pragma omp for
			for(i = 0; i < nb_nodes; i++) {
			
				foutput << i <<";" << map_communities[i] << ";";
				foutput << z_intensite(i, true) << ";" << z_intensite(i, false) << ";" << endl;
				if (cpt % 200000 == 0)
					cout << thread_id <<" a traité 200000 noeuds" << endl; 
				cpt++;

			}
			foutput.close();
			cpt=1;
			#pragma omp barrier
			#pragma omp master 
			{
				//Maintenant, l'homogénéité
					cout << "Calcul des moyennes et ecart type pour l'homogénéité " <<endl;
			}
			//Maintenant, l'homogénéité
			#pragma omp for
			for(i = 0; i < communities.size(); i++) {
				avg_com_out[i]= avg_homogeneite(i, true);
				std_com_out[i]= standard_deviation_homogeneite(i, true);
				avg_com_in[i] = avg_homogeneite(i, false);
				std_com_in[i] = standard_deviation_homogeneite(i, false);
				if (cpt % 500 == 0)
					cout << thread_id <<" a traité 500 communautés" << endl; 
				cpt++;
	
			}
			cpt=1;
			#pragma omp barrier
			#pragma omp master 
			{
				//Maintenant, l'homogénéité
					cout << "Calcul de l'homogénéité " <<endl;
			}
			filename2 = filename+"_homogeneite_" + ss.str();
			//cout << filename2 << endl;
			foutput.open((filename2).c_str(), fstream::out);
			#pragma omp for
			for(i = 0; i < nb_nodes; i++) {
			
				foutput << i <<";" << map_communities[i] << ";";
				foutput << z_homogeneite(i, true) << ";" << z_homogeneite(i, false) << ";"<<endl;	
				if (cpt % 200000 == 0)
					cout << thread_id <<" a traité 200000 noeuds" << endl; 
				cpt++;

			}
			foutput.close();
		}


	}


	
