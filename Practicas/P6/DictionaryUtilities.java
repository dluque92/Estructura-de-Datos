/**
 * PRACTICA 6ª. 
 *
 * Estructuras de Datos. 2º Curso. ETSI Informática. UMA
 *
 * El objetivo de esta práctica es manejar la estructura diccionario, sus iteradores 
 * y diseñar algunas utilidades. 
 * Completa los métodos:  inverseDictionary , keysWithMaximaValue (la descripción 
 * de cada uno de éstos aparece tras su cabecera).
 * 
 * El método main contiene intrucciones para comprobar la corrección de la solución. 
 * 
 * Puesto que se visualizan los diccionarios ordenados por claves, 
 * el conjunto asociado a la clave mayor debe figurar en
 * último elemento del diccionario inverso.
 *
 * (completa y sustituye los siguientes datos)
 * Titulación: Grado en Ingeniería …………………………………… [Informática | del Software | de Computadores].
 * Alumno: APELLIDOS, NOMBRE
 * Fecha de entrega:  DIA | MES | AÑO
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
 * Devuelve el diccionario inverso del diccionario argumento; en éste diccionario inverso a cada valor le hace corresponder   
 * el conjunto de claves (del diccionario argumento) que tenía dicho valor. Por ejemplo, para el 
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
 * el conjunto de claves con valor máximo es:
 *
 * AVLSet(ac,cc)
 */
		Set<K> maxima = new AVLSet<>(); // el conjunto de claves con valor máximo
 
		V maxV = null; // el valor máximo  

// Hay que completar aquí el recorrido sobre el diccionario 
// a través de un iterador. 
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
//		Dictionary<String,Integer> d = randomDictionary(0, 0); // genera un diccionario vacío

        Dictionary <Integer,Set<String>> di = inverseDictionary (d);

		System.out.println("*** Un diccionario aleatorio nombre -> edad :");
		System.out.println(d.toString());

		System.out.println("*** El conjunto de claves con valor máximo:");
		System.out.println( keysWithMaximaValue(d).toString());	

		System.out.println("*** El diccionario inverso:");
		System.out.println(di.toString());	
	}

}