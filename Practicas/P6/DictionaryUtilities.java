/**
 * PRACTICA 6�. 
 *
 * Estructuras de Datos. 2� Curso. ETSI Inform�tica. UMA
 *
 * El objetivo de esta pr�ctica es manejar la estructura diccionario, sus iteradores 
 * y dise�ar algunas utilidades. 
 * Completa los m�todos:  inverseDictionary , keysWithMaximaValue (la descripci�n 
 * de cada uno de �stos aparece tras su cabecera).
 * 
 * El m�todo main contiene intrucciones para comprobar la correcci�n de la soluci�n. 
 * 
 * Puesto que se visualizan los diccionarios ordenados por claves, 
 * el conjunto asociado a la clave mayor debe figurar en
 * �ltimo elemento del diccionario inverso.
 *
 * (completa y sustituye los siguientes datos)
 * Titulaci�n: Grado en Ingenier�a �������������� [Inform�tica | del Software | de Computadores].
 * Alumno: APELLIDOS, NOMBRE
 * Fecha de entrega:  DIA | MES | A�O
 */

import java.util.Random;
import java.util.Iterator;

import dataStructures.dictionary.*;
import dataStructures.set.*;
import dataStructures.tuple.Tuple2;


public class DictionaryUtilities  {

	public static  <
				K extends Comparable<? super K>, 
				V extends Comparable<? super V> > 
	          Dictionary < V,Set<K> > inverseDictionary ( Dictionary<K,V> dicKv) {
/*
 * Devuelve el diccionario inverso del diccionario argumento; en �ste diccionario inverso a cada valor le hace corresponder   
 * el conjunto de claves (del diccionario argumento) que ten�a dicho valor. Por ejemplo, para el 
 * diccionario aleatorio nombre -> edad :
 *
 * AVLDictionary(aa->4,ab->1,ac->5,ba->2,bb->2,bc->1,ca->2,cb->3,cc->5)
 *
 * el diccionario inverso debe ser:
 * 
 * AVLDictionary(1->AVLSet(ab,bc),2->AVLSet(ba,bb,ca),3->AVLSet(cb),4->AVLSet(aa),5->AVLSet(ac,cc))
 */
		Dictionary<V,Set<K>> dicVk = new AVLDictionary<>();
		// sobre este diccionario
		
		for( Tuple2<K,V> kvTuple : dicKv.keysValues()){

// hay que completar este bucle
// ...
		}
		return dicVk;
	}

	public static  <
				K extends Comparable<? super K>, 
				V extends Comparable<? super V> > 
		 	Set<K> keysWithMaximaValue ( Dictionary<K,V> dicKv) {
/*
 * Devuelve el conjunto de claves con que tienen mayores valores en el diccionario argumento
 * Por ejemplo, para el diccionario aleatorio nombre -> edad :
 *
 * AVLDictionary(aa->4,ab->1,ac->5,ba->2,bb->2,bc->1,ca->2,cb->3,cc->5)
 * el conjunto de claves con valor m�ximo es:
 *
 * AVLSet(ac,cc)
 */
		Set<K> maxima = new AVLSet<>(); // el conjunto de claves con valor m�ximo
 
		V maxV = null; // el valor m�ximo  

// Hay que completar aqu� el recorrido sobre el diccionario 
// a trav�s de un iterador. 
// En este bucle iremos actualizando convenientemente las variables maxima y maxV
// ...

		return maxima;
	}


	public static Dictionary<String,Integer> randomDictionary(int seed, int size) {

		Dictionary<String,Integer> d = new AVLDictionary<>(); 
        Random rnd = new Random();
        if (seed != 0) rnd = new Random(seed);
        String cadena = "abc";
        final int N_CHAR = 2;
        final int EDAD_MAX = 10;
        
		for(int i=0; i<size; i++) {
			int edad = rnd.nextInt(EDAD_MAX);
			String nombre = "";
			for (int j=0; j<N_CHAR; j++)
				nombre += cadena.charAt(rnd.nextInt(cadena.length()));
			d.insert(nombre,edad);
			} 
		return d;
		}
	

	public static void main(String[] args) {

		
		Dictionary<String,Integer> d = randomDictionary(0, 100); 
//		Dictionary<String,Integer> d = randomDictionary(0, 0); // genera un diccionario vac�o

        Dictionary <Integer,Set<String>> di = inverseDictionary (d);

		System.out.println("*** Un diccionario aleatorio nombre -> edad :");
		System.out.println(d.toString());

		System.out.println("*** El conjunto de claves con valor m�ximo:");
		System.out.println( keysWithMaximaValue(d).toString());	

		System.out.println("*** El diccionario inverso:");
		System.out.println(di.toString());	
	}

}