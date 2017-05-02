/********************************************************************
 * Estructuras de Datos. 2º Curso. ETSI Informática. UMA
 * PRACTICA 4ª. Ejercicio 12.b de la tercera relación
 *              Implementar el TAD Bolsa en java
 *
 * (completa y sustituye los siguientes datos)
 * Titulación: Grado en Ingeniería [Informática | del Software | de Computadores].
 * Alumno: APELLIDOS, NOMBRE
 * Fecha de entrega:  DIA | MES | AÑO
 ********************************************************************
 */

package dataStructures.bag;

public class LinkedBag<T extends Comparable<? super T>> implements Bag<T> {

	static private class Node<E> {
		E elem;
		int count;
		Node<E> next;

		Node(E x, int n, Node<E> node) {
			elem = x;
			count = n;
			next = node;
		}
	}

	private Node<T> first; // keep the linked list sorted by "elem"

	public LinkedBag() {
		first = null;
	}

	public boolean isEmpty() {

		// TO BE REPLACED WITH APPROPRIATE BOOLEAN EXPRESSION

		return true;
	}

	public void insert(T item) {
		Node<T> previous = null;
		Node<T> current = first;

		while (current != null && current.elem.compareTo(item) < 0) {
			previous = current;
			current = current.next;
		}

		if (current != null && current.elem.equals(item)) {

			// TO BE FILLED OUT

		} else if (previous == null) {
			first = new Node<T>(item, 1, first);
		} else {

			// TO BE FILLED OUT

		}
	}

	public int occurrences(T item) {
		Node<T> current = first;
		int result = 0;

		while (current != null && current.elem.compareTo(item) < 0) {
			current = current.next;
		}

		if (current != null && current.elem.equals(item)) {

			// TO BE FILLED OUT

		}
		return result;
	}

	public void delete(T item) {
		Node<T> previous = null;
		Node<T> current = first;

			// TO BE FILLED OUT

	}

	public String toString() {
		String text = "Bag(";
		for (Node<T> p = first; p != null; p = p.next) {
			text += "(" + p.elem + ", " + p.count + ") ";
		}
		return text + ")";
	}
}
