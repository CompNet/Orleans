#include<string.h>
#include "../include/graph.h"

using namespace std;

Graph::Graph(char *filename, int type, bool do_renumber) {
  ifstream finput;
  finput.open(filename,fstream::in); 
  if(do_renumber) {	
  
  unsigned int src, dest, cpt;
  cpt = 0;

	unsigned int src_prec, dest_prec;
        src_prec = -1;
        dest_prec = -1;
        
  int nb_links=0;
  double weight = 1.;
  ofstream foutput;
	char* tmp = new char[strlen(filename) + 6];
	strcat(tmp, filename);
	strcat(tmp, "_renum");
  foutput.open(tmp, fstream::out | fstream::binary);

		cout << "Renumerotation begins..." << endl;
		
		correspondance.resize(0);

	/* Find minimum and maximum value of ids */
      unsigned int min, maxi;

        if(type == WEIGHTED)
                finput >> src >> dest >> weight;
        else
                finput >> src >> dest;

        min = src;
        maxi = dest;

        while(!finput.eof()) {
		nb_links++;
                finput >> src >> dest;

                if(min > src)
                        min = src;
                if(min > dest)
                        min = dest;

                if(maxi < src)
                        maxi = src;
                if(maxi < dest)
                        maxi = dest;
		if(nb_links % 1000 == 0) cout << "10000000 lus" << endl;

        }

        finput.close();
	cout << "Maximum found : " << maxi << endl;

        /* Creating a bunch of maps */
        vector<map<unsigned int, unsigned int> > corres;
        int interval = 1000;

	corres.resize((maxi/interval)+1);
        unsigned int nb_interval_src, nb_interval_dest;

        finput.open(filename,fstream::in);

        if(finput) {

        while(!finput.eof()) {

			if(finput.eof()) 

				break;

                        if(type == WEIGHTED)
                                finput >> src >> dest >> weight;
                        else
                                finput >> src >> dest;

			 			if(src_prec == src && dest_prec == dest)

                                continue;

			nb_interval_src = src / interval;
                        if(corres[nb_interval_src].find(src) == corres[nb_interval_src].end()) {
				corres[nb_interval_src].insert(make_pair(src, cpt));
                                correspondance.resize(correspondance.size()+1);
                                correspondance[cpt] = src;
                                cpt++;

                        }

			nb_interval_dest = dest / interval;
                        if(corres[nb_interval_dest].find(dest) == corres[nb_interval_dest].end()) {
				corres[nb_interval_dest].insert(make_pair(dest, cpt));
                                correspondance.resize(correspondance.size()+1);
                                correspondance[cpt] = dest;
                                cpt++;

                        }
                        
					    
					    	map<unsigned int, unsigned int>::iterator it_src;
					    	map<unsigned int, unsigned int>::iterator it_dest;
					    
					    	it_src = corres[nb_interval_src].find(src);
                                                it_dest = corres[nb_interval_dest].find(dest);

						unsigned int pos_src = it_src->second;
						unsigned int pos_dest = it_dest->second;
	
					      if (links_out.size()<=max(pos_src,pos_dest)+1) {
					        links_out.resize(max(pos_src,pos_dest)+1);
					      }
						if(links_in.size()<=max(pos_src,pos_dest)+1) {
							links_in.resize(max(pos_src,pos_dest)+1);
						}
					      
					      links_out[pos_src].push_back(make_pair(pos_dest,weight));
					      links_in[pos_dest].push_back(make_pair(pos_src,weight));
					
					      nb_links++;
	
		// TODO: count nb_links while parsing for the maximum, and pourcent the progression 			
		if(nb_links % 10000000 == 0) cout << "10000000 ecrits" << endl;

						foutput << pos_src << " " << pos_dest << endl;

                     	src_prec = src;
                        dest_prec = dest;

                }

        }
	finput.close();

	cout << "Renumerotation ends..." << endl;
	foutput.close();


}
else {
int src, dest, cpt;
  cpt = 0;

        int src_prec, dest_prec;
        src_prec = -1;
        dest_prec = -1;
        
  int nb_links=0;
  float weight = 1.;

                
        if(finput) {

        while(!finput.eof()) {

                        if(finput.eof())

                                break;

                        if(type == WEIGHTED)
                                finput >> src >> dest >> weight;
                        else
                                finput >> src >> dest;

                                                if(src_prec == src && dest_prec == dest)

                                continue;

			if (links_out.size()<=max(src,dest)+1) {
                                                links_out.resize(max(src,dest)+1);
                                              }
                                                if(links_in.size()<=max(src,dest)+1) {
                                                        links_in.resize(max(src,dest)+1);
                                                }
                                              
                                        
                                              links_out[src].push_back(make_pair(dest,weight));
                                              links_in[dest].push_back(make_pair(src,weight));
                                              /* TODO: rajouter links_out_in si on veut aussi les voisins entrants */
                                              /* Duplication du lien. Genant pour les orientes */
                                              //if (src!=dest)
                                              //        links_out[dest].push_back(make_pair(src,weight));
                                        
                                              nb_links++;

                        src_prec = src;
                        dest_prec = dest;

}}}
    
  finput.close();

}


/* Permet de gerer les multigraphes : si on a dans le fichier 1 4 3 et 1 4 6, 
 * le graphe original aura deux paires (4,3) et (4,6) dans la liste d'adjacence 
 * de 1. Du coup, cette procedure remplacera ca par (4,9). 
 * UTILE UNIQUEMENT EN ORIENTE, DONC INUTILE.
 */

void
Graph::display(int type) {
  for (unsigned int i=0 ; i<links_out.size() ; i++) {
    for (unsigned int j=0 ; j<links_out[i].size() ; j++) {
      int dest   = links_out[i][j].first;
      float weight = links_out[i][j].second;
      if (type==WEIGHTED)
	cout << i << " " << dest << " " << weight << endl;
      else
	cout << i << " " << dest << endl;
    }
  }
}

void
Graph::displayFile(string filename, int type, bool renumber) {
  ofstream foutput;
  foutput.open(filename.c_str(), fstream::out);
  for (unsigned int i=0 ; i<links_out.size() ; i++) {
    for (unsigned int j=0 ; j<links_out[i].size() ; j++) {
      unsigned int dest   = links_out[i][j].first;
      float weight = links_out[i][j].second;
      if (type==WEIGHTED)
		if (renumber)
			foutput << correspondance[i] << " " << correspondance[dest] << " " << weight << endl;
		else 
			foutput << i << " " << dest << " " << weight << endl;
      else
		if (renumber)	
			foutput << correspondance[i] << " " << correspondance[dest] << endl;
		else
			foutput << i << " " << dest << endl;
    }
  }
  foutput.close();
}

void
Graph::display_binary(char *filename, char *filename_w, int type, bool do_renumber) {
  ofstream foutput;
  foutput.open(filename, fstream::out | fstream::binary);
  cout << "ecriture" << endl;
  unsigned int s = links_out.size();

  // outputs number of nodes
  foutput.write((char *)(&s),sizeof(int));

  // outputs cumulative degree sequence
  /* Contient uniquement les degres sortants en oriente */
  long tot=0;
  for (unsigned int i=0 ; i<s ; i++) {
      tot+=(long)links_out[i].size();
    foutput.write((char *)(&tot),sizeof(long));
  }

  // outputs cumulative degree sequence
  /* Contient uniquement les degres entrants en oriente */
  long tot_in=0;
  for (unsigned int i=0 ; i<s ; i++) {
        tot_in+=(long)links_in[i].size();
    foutput.write((char *)(&tot_in),sizeof(long));
  }

  // outputs correspondance
  if(do_renumber) {
  for(unsigned int i = 0; i <s ; i++) {

                unsigned int corr = correspondance[i];
                foutput.write((char *)(&corr),sizeof(int));

  } }

  // outputs links_out
  for (unsigned int i=0 ; i<s ; i++) {
    for (unsigned int j=0 ; j<links_out[i].size() ; j++) {
      unsigned int dest = links_out[i][j].first;
      foutput.write((char *)(&dest),sizeof(int));
    }
  }

  // outputs links_in
  for (unsigned int i=0 ; i<s ; i++) {
    for (unsigned int j=0 ; j<links_in[i].size() ; j++) {
      unsigned int dest = links_in[i][j].first;
      foutput.write((char *)(&dest),sizeof(int));
    }
  }
  foutput.close();
  
  // outputs weights in a separate file
  if (type==WEIGHTED) {
    ofstream foutput_w;
    foutput_w.open(filename_w,fstream::out | fstream::binary);
    for (unsigned int i=0 ; i<s ; i++) {
      for (unsigned int j=0 ; j<links_out[i].size() ; j++) {
        float weight = links_out[i][j].second;
        foutput_w.write((char *)(&weight),sizeof(int));
      }
    }
    foutput_w.close();
  }
}


