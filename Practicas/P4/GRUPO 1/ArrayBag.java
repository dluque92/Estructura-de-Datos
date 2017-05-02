/********************************************************************
 * Estructuras de Datos. 2º Curso. ETSI Informática. UMA
 * PRACTICA 4ª. Ejercicio 12.c de la tercera relación
 *              Implementar el TAD Bolsa en java
 *
 * (completa y sustituye los siguientes datos)
 * Titulación: Grado en Ingeniería [Informática | del Software | de Computadores].
 * Alumno: APELLIDOS, NOMBRE
 * Fecha de entrega:  DIA | MES | AÑO
 ********************************************************************
 */

package dataStructures.bag;

import java.util.Arrays;

public class ArrayBag<T extends Comparable<? super T>> implements Bag<T> {

	private final static int INITIAL_CAPACITY = 5;

	protected T[] value; // keep this array sorted
	protected int[] count; // keep only positive counters
	protected int nextFree;

	public ArrayBag() {
		this(INITIAL_CAPACITY);
	}

	@SuppressWarnings("unchecked")
	public ArrayBag(int n) {
		value = (T[]) new Comparable[n]; // everything to null
		count = new int[n]; // everything to 0
		nextFree = 0;
	}

	private void ensureCapacity() {
		if (nextFree == value.length) {
			value = Arrays.copyOf(value, 2 * value.length);

			// TO BE FILLED OUT

		}
	}

	public boolean isEmpty() {
		return nextFree == 0;
	}

	// if "item" is stored in the array "value", returns its index
	// otherwise returns the index where "item" would be inserted

	private int locate(T item) {
		int lower = 0;
		int upper = nextFree - 1;
		int mid = 0;
		boolean found = false;

		// binary search
		while (lower <= upper && !found) {
			mid = lower + ((upper - lower) / 2); // == (lower + upper) / 2;
			found = value[mid].equals(item);
			if (!found) {
				if (value[mid].compareTo(item) > 0) {
					upper = mid - 1;
				} else {
					lower = mid + 1;
				}
			}
		}

		if (found)
			return mid; // the index where "item" is stored
		else
			return lower; // the index where "item" would be inserted
	}

	public void insert(T item) {
		ensureCapacity();
		int i = locate(item);
		if (value[i] != null && value[i].equals(item)) {

			// TO BE FILLED OUT

		} else {
			// shift elements to right
			for (int j = nextFree; j > i; j--) {
				value[j] = value[j - 1];
				count[j] = count[j - 1];
			}

			// TO BE FILLED OUT

		}
	}

	public int occurrences(T item) {
		int result = 0;
		int i = locate(item);
		if (value[i] != null && value[i].equals(item)) {

			// TO BE FILLED OUT

		}
		return result;
	}

	public void delete(T item) {
		int i = locate(item);
        
            // TO BE FILLED OUT

	}

	public String toString() {
		String text = "Bag(";
		for (int i = 0; i < nextFree; i++) {
			text += "(" + value[i] + ", " + count[i] + ") ";
		}
		return text + ")";
	}
}
