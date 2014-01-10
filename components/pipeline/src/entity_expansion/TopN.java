package entity_expansion;

import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.List;
import java.util.Map.Entry;
import java.util.Comparator;
import java.util.PriorityQueue;

/**
 * Stores the n biggest (by score) elements only.
 */
public class TopN<T extends Comparable<T>> {
  private PriorityQueue<Entry<T, Double>> elements;
  private int n;
  public TopN(int n) {
    this.n = n;
    this.elements = new PriorityQueue<Entry<T, Double>>(11,
        new Comparator<Entry<T, Double>>() {
          public int compare(Entry<T, Double> o1, Entry<T, Double> o2) {
            if (o1.getValue() > o2.getValue()) return 1;
            if (o1.getValue() < o2.getValue()) return -1;
            return o1.getKey() == null ? 1 : o1.getKey().compareTo(o2.getKey());
          }
    });
  }
  
  /**
   * Adds the element if the score is higher than the n-th smallest score.
   */
  public void add(T element, double score) {
    Entry<T, Double> keyVal = new SimpleEntry<T, Double>(element,score);
    elements.add(keyVal);
    if (elements.size() > n) {
      elements.poll();
    }
  }
  
  /**
   * Returns the elements with n biggest scores.
   */
  public PriorityQueue<Entry<T, Double>> get() {
    return elements;
  }
  
  public List<T> elementList() {
    List<T> l = new ArrayList<T>(elements.size());
    for (Entry<T, Double> e : elements) {
      l.add(e.getKey());
    }
    return l;
  }
  
  public static void main(String[] args) {
    TopN<String> tn = new TopN<String>(2);
    tn.add("a", 5);
    tn.add("a", 4);
    tn.add("b", 5);
    tn.add("b", 2);
    tn.add("c", 6);
    tn.add("d", 1);
    System.out.println(tn.elementList());//"[b, c]"
  }
  
}
