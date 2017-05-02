/**
 * APELLIDOS :                         NOMBRE:
 *
 * TITULACION: .
 *
 * Computes Topological Sorting for DiGraphs
 */

package dataStructures.graph;

import dataStructures.dictionary.Dictionary;
import dataStructures.dictionary.HashDictionary;
import dataStructures.list.List;
import dataStructures.list.ArrayList;
import dataStructures.queue.Queue;
import dataStructures.queue.LinkedQueue;

public class TopologicalSortingDic<V> {

    private List<V> topSort;
    private boolean hasCycle;

    public TopologicalSortingDic(DiGraph<V> graph) {

        topSort = new ArrayList<V>();
        // dictionary: vertex -> # of pending predecessors
        Dictionary<V, Integer> pendingPredecessors = new HashDictionary<V, Integer>();
        Queue<V> sources = new LinkedQueue<V>();

        // completar
     }

    public boolean hasCycle() {
        return hasCycle;
    }

    public List<V> order() {
        return hasCycle ? null : topSort;
    }
}
