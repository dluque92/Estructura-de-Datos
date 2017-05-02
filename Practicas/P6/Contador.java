import dataStructures.set.*;
import dataStructures.dictionary.*;

public class Contador {

	public static void main(String[] args) {
		String[] lineas = {
				"IT WAS the best of times, it was the worst of times,",
				"it was the age of wisdom, it was the age of foolishness,",
				"it was the epoch of belief, it was the epoch of incredulity,",
				"it was the season of Light, it was the season of Darkness,",
				"it was the spring of hope, it was the winter of despair,",
				"we had everything before us, we had nothing before us,",
				"we were all going direct to Heaven, we were all going direct the other way",
				"- in short, the period was so far like the present period, that some of",
				" its noisiest authorities insisted on its being received,",
				"for good or for evil, in the superlative degree of comparison only." };

		// para cada letra, calcular las lineas en las que aparece y la posición
		// dentro de cada línea
		Dictionary<Character, Dictionary<Integer, Set<Integer>>> contador =
                                        new AVLDictionary<>();

		// Construir aqui el contador
		
		
		
		for (char c : contador.keys()) {
			System.out.println(c);
			for (int i : contador.valueOf(c).keys()) {
				System.out.print(i  +": ");
				for (int j : contador.valueOf(c).valueOf(i)) {
					System.out.print(" " +j );
				}
				System.out.println();
			}
		}
	}
	
/*
 * Salida para el ejemplo dado
A
0:  4
D
3:  49
H
6:  28
I
0:  0
L
3:  21
S
0:  5
T
0:  1
W
0:  3
a
0:  30
1:  4 11 30 37
2:  4 32
3:  4 13 32 41 50
4:  4 31 52
5:  4 33
6:  8 30 44 72
7:  24 31 61
8:  14
9:  35 55
b
0:  11
2:  20
5:  18 44
8:  42
c
2:  14 42 50
6:  22 58
8:  50
9:  51
d
1:  21
2:  53
4:  48
5:  5 34
6:  18 54
7:  21 56
8:  33 55
9:  7 41
e
0:  9 12 22 35 49
1:  9 13 35 39 52
2:  9 11 21 24 37 39 52
3:  9 12 37 40 54
4:  9 24 36 42 49
5:  1 7 9 19 23 30 45 49
6:  1 4 6 21 29 32 37 40 42 57 63 68
7:  14 17 37 41 45 47 52 67
8:  10 23 32 43 49 51 54
9:  16 27 32 39 42 45 46
f
0:  17 44
1:  16 42 44
2:  18 25 46
3:  19 47
4:  19 46
5:  20 46
7:  30 70
9:  0 12 49
g
1:  12 38
3:  23
4:  16
5:  16 42
6:  12 16 48 52
8:  46
9:  4 43
h
0:  8 34
1:  8 34 50
2:  8 15 36 43
3:  8 24 36
4:  8 21 35
5:  3 13 32 39
6:  62 67
7:  6 13 40 60
8:  17
9:  26
i
0:  20 26 47
1:  0 19 26 48
2:  0 23 28 48 56
3:  0 22 28
4:  0 14 27 39 53
5:  14 40
6:  14 19 50 55
7:  2 19 35 54
8:  1 7 9 20 22 26 29 38 44 52
9:  18 22 37 57
k
3:  52
7:  36
l
1:  47
2:  22 55
6:  9 10 45 46
7:  34
9:  19 34 64
m
0:  21 48
1:  23
7:  66
9:  53
n
1:  51
2:  49
3:  16 44 53
4:  15 40
5:  15 36 41
6:  15 33 51
7:  3 48
8:  5 27 36 45
9:  23 60 63
o
0:  16 38 43
1:  15 22 41 45 46
2:  13 17 41 45
3:  15 18 43 46
4:  18 22 45
5:  21 37 47
6:  13 26 49 65
7:  7 20 28 55 65 69
8:  6 18 35
9:  1 5 6 9 13 48 52 59 62
p
2:  12 40
4:  12 23 51
7:  16 43 51
9:  31 54
r
0:  39
2:  51
3:  51
4:  13 43 54
5:  10 22 48
6:  5 20 41 56 69
7:  8 18 32 44 53
8:  19 48
9:  2 10 14 33 44 56
s
0:  13 23 31 40 50
1:  5 20 31 49 53 54
2:  5 33
3:  5 11 14 33 39 42 55 56
4:  5 11 32 50
5:  26 52
7:  5 25 27 46 64
8:  3 8 11 24 28 30 40
9:  29 58
t
0:  7 14 19 27 33 41 46
1:  1 7 27 33
2:  1 7 29 35 57
3:  1 7 25 29 35
4:  1 7 28 34 41
5:  12 38
6:  23 25 59 61 66
7:  9 12 39 49 59 62
8:  2 12 16 21 31 39
9:  25 36
u
2:  54
5:  25 51
8:  15
9:  30
v
5:  8
6:  31
8:  53
9:  17 38
w
0:  29 37
1:  3 18 29
2:  3 31
3:  3 31
4:  3 30 38
5:  0 29
6:  0 3 36 39 71
7:  23
y
2:  58
5:  11
6:  73
9:  65
*/
}
