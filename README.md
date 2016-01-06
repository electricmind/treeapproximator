treeapproximator
================
TreeApproximator is a tool that allows to arrange a set of files (text or html ones) into a tree, that can be represented as catalog or html-file. It uses our own version of Hierarchical Clustering.

Hierarchical Clustering
-------------------------
Hierarchical clustering arranges similar pages into a tree of groups, where each group defines a central vector. An algorithm starts with a group of two vectors, where central vector is their average. For each new vector it looks for the closest vector recursively choosing the closest central vector until until it encounters a leaf, which gets converted to a new pair of vectors. The algorithm updates central vector for every inner node on a path to the new vector.  For each new vector a learning step randomly removes a couple of vectors and inserts them back  to a tree (that leads to minimization of error).

This approach is similar to and really is an improvment over K-means. Theoretically,  it has n*log n complexity but on real data a tree becomes unbalanced and dimension grows a lot that turns complexity up to  n**1.5.

At the final stage algorithm alignes vectors and  joins clusters together on the base of an  accuracy coefficient that is more easy to choose than an amount of clusters. Later, this coefficient can be changed without repeating computation for the entire data corpus.

By means of maintaining an actual set of clusters from the beginning, the algorithm makes on-line learning possible makes on-line learning possible.

Usage
-------
An outcome of clustering technique is a bunch of articles that are arranged 
into a few clusters, and, by product, aligned in a sequence of mutually similar items. Current implementation also uses a simple technique to elicit keywords that distinguish clusters from each other.

Current version is still incomplete and requires some redundant computation, but can be used as follows:

  `java -jar treeapproximator-assembly-1.3.jar [links|clusters|tree] <Directory> [<List of files>]`, where:

  * "links" dumps a page of references to the Wikipedia (for lack of a preliminary wishes it's just a desperate attempt to present an outcome, but looks fine),

  * "clusters" stores clusters of pages in separate directories,  titled by discernible keywords and equipped with a positional vectors;

  * "tree" arranges pages into a tree of  folders, each containing two subfolders along with a positional vector of its subcluster.
   
An additional tool `java -cp webcrawler-assembly-1.1.jar ru.wordmetrix.treeapproximator.Draw2DMap [<File of Points>]` is a simple demo that 
generates a few clouds of gaussian-distributed points and builds a tree.

The outcome
-----------
I used this approach to arrange 
[3500 pages that are adjacent to the page "Algorithm" of Wikipedia](http://electricmind.github.io/demo/treeapproximator/2013-11-04-an-outcome-of-hierarchical-clustering-of-wikipedia-pages.html)


History
---------
Version 1.0, 20131101, Initial release as a part of webcrawler

Version 1.1, 20140114, Use akka.actor, build with sbt and make a few enhancements

Version 1.2, 20140129, Move libraries out, implement ArrangeText as independent tool and factor out priority forecasting code, add initialization with  multiply seeds
and permission to cross intersite links.

Version 1.3, 20150129, Factor out ArrangeText tool into a separate repository.


