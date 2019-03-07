(*Practice questions mostly taken from https://kwarc.info/teaching/GenCS/sml-tutorial.pdf*)

fun nth_helper(L,n,acc)= 
    if L = [] then raise Subscript			(*raise exception if n > length of L*)
    else 
        if n=acc then hd(L)
        else nth_helper(tl(L),n,acc+1);
   
fun nth(L,n) = 						(*return nth element of a list*)
    if n < 0 then raise Subscript			(*index starts from zero, exception if n<0*)
    else nth_helper(L,n,0);

fun take(xs,n) = 					(*return first n elements of a list*)
    if n < 0 then raise Subscript
    else if n = 0 then []
    else if xs = [] then raise Subscript
    else hd(xs)::take(tl(xs),n-1);

fun drop(xs,n) = 
    if n < 0 then raise Subscript
    else if n = 0 then xs
    else if xs = [] then raise Subscript
    else drop(tl(xs),n-1);

fun zip(L1,L2) = 					(*Take two lists and return ordered tuples*)
    if L1 = [] then [] 					(*for example, [1,3] and [2,4]= [[1,2], [3,4] ]*)
    else if L2 = [] then []
    else 
        [hd(L1),hd(L2)]::zip(tl(L1),tl(L2));

fun unzip_helper(L,n) = 
    if L = [] then []
    else nth((hd(L)),n)::unzip_helper(tl(L),n);

fun unzip(L) = [unzip_helper(L,0)]@[unzip_helper(L,1)]; 	(*reverse zip effect*)

(*Another solution of unzip: flatten list, odd elements become one list, even elements becoem one list*)

fun range(n,m) = 				(*returns a list of elements [n,n+1,n+2,...m]*)
    if n > m then raise Empty
    else if n = m then [m]
        else n::range(n+1,m);

fun length(L) = 				(*find length of a list*)
    if L = [] then 0
    else 1+ length(tl(L));

fun rev(L) = 					(*reverse a list*)
    if L = [] then []
    else rev(tl(L))@[hd(L)];

fun last_n(L,n) = 					(*take last n elements*)
    if n < 0 then raise Subscript
    else drop(L,length(L)-n);			(*drop first length - n elements*)


(*For the decimal addition function, we represent natural numbers as lists of of digits, e.g. [5,3,4] for the number 534. Now the function badd works as follows: badd([2,8],[1,3]) evaluates to [4,1]. The binary addition function is similar, only have it operates on lists of binary digits, i.e. the numbers 1 and 0. *)

(*The idea is to add from right to left (could work with a reversed list), and keep account of the carry, either 0 or 1 and add it to the result. Keep in mind that when reversing a list, or adding elements from left to right, the format has to be recursive_function @ [element]. I added padding zeros in case the two lists have different length digits. *)

fun pad(n) = 								(*returns a list of n zeros*)
    if n < 0 then []
    else if n = 0 then []
    else
        0::pad(n-1);
        
fun carry(L1,L2,car) = 
    if L1 = [] then []
    else if L2 = [] then []
    else 
        if hd(L1)+hd(L2)>=10
            then  carry(tl(L1),tl(L2),1) @ [hd(L1)+hd(L2)+car-10]
        else
            carry(tl(L1),tl(L2),0) @ [hd(L1)+hd(L2)+car];
    
fun badd(L1,L2) = 							(*perform addition of digits*)
    if length(L1) > length(L2) then					(* [1] + [2,1] = [0,1] + [2,1]*)
        carry(rev(L1),rev(pad(length(L1)-length(L2))@L2),0)
    else
        carry(rev(pad(length(L2)-length(L1))@L1),rev(L2),0);

(*For adding binary digits ( e.g [1111] + [1111], the carry occurs when digit exceeds 2 and we have to take care of the overflow case where the most sig bit >2). All we have to do is change the carry helper function to the following*)

fun binary_carry(L1,L2,car) = 
    if L1 = [] then [car]		        (*at the most sig bit, append carry to take care of overflow*)
    else if L2 = [] then [car]
    else 
        if hd(L1)+hd(L2)+car>=2 
            then  binary_carry(tl(L1),tl(L2),1) @ [hd(L1)+hd(L2)+car-2]
        else
            binary_carry(tl(L1),tl(L2),0) @ [hd(L1)+hd(L2)+car];



(*Implementation of merge_sort
Note: my implementation is a little different from the lecture slides 
       Instead of invoking take and skip, where take gets odd numbered elements and skip takes even numbered elements, I split the list into the first half with take, and the second half with drop (drops the first half). The difference is actually trivial, since our goal is to split the list into halves, disregarding how we decide to split. *)

fun merge_helper([],L2) = L2			(*merge function using priority queue*)
| merge_helper(L1,[]) = L1			
| merge_helper(x::L1,y::L2) = 
    if x>y then y::merge_helper(x::L1,L2)
    else x::merge_helper(L1,y::L2);

fun merge_sort(L) =
    if L = [] then []
    else if tl(L) = [] then L
    else
        merge_helper(merge_sort( take(L,length(L) div 2) ),merge_sort( drop(L,length(L) div 2)) );


fun flat(L) = 					(*flattens a 2d list*)
    if L = [] then []		 (*[1,[1,2]] is not possible since type of elements dont agree*)
    else
        hd(L)@flat(tl(L));




fun last(L) = 				(*return the last element of the list*)
    if L = [] then 0
    else if tl(L) = [] then hd(L)
    else last(tl(L));

fun removeLast(L) = 			(*remove last element from the list*)
    if L = [] orelse tl(L) = [] then []
    else hd(L)::removeLast(tl(L));
    
fun palindrome(L) = 
    if L = [] orelse tl(L) = [] then true
    else if hd(L) = last(L) then palindrome(removeLast(tl(L)))
    else false;

(*Alternate easier solution *)
fun palindrome(L) = rev(L) = L;

fun pack_helper(L,acc) = 
    if L = [] orelse tl(L) = [] then [acc]
    else if hd(L) = hd(tl(L)) then pack_helper(tl(L),hd(L)::acc)
    else
        [acc]@pack_helper(tl(L),[hd(tl(L))]);

fun pack(L) = 
    if L = [] then []
    else pack_helper(L,[hd(L)]);

(*pack consecutive duplicates of list elements into sublists
   [1,1,1,2,3] would return [  [1,1,1] , [2] , [3]  ] *)

fun encode_helper(L) = 
    if L = [] 
        then []
    else 
        [(   length(hd(L)),hd(hd(L))   )] @ encode_helper(tl(L));
                  
fun encode(L) = 
    encode_helper( pack(L) );

(*we can also do it without the pack function, just count number of duplicates using an accumulator 
Encode can also be implemented using a map higher-level function*)

fun map(f,n) = 
    if n = [] then []
    else
        f(hd(n)) :: map(f,tl(n));

fun encode1(L) = map(fn x => (length(x),hd(x)) ,pack(L));


fun uncompress(n,c) = 
    if n = 0 then []
    else c::uncompress(n-1,c);
    
fun decode([]) = []
| decode( (x,y)::L) = 
    uncompress(x,y)@decode(L);		

(*taking something in form [ (4,’a’) , (5,’b’)] and expand it into [a,a,a,a,b,b,b,b,b]*)


fun member(x,l) = 			(*return true if x exists in l*)
    if l = [] then false
    else if x = hd(l) then true
    else member(x,tl(l));
	
fun union(L1,L2) = 			(*return union of 2 lists*)
    if L1 = [] then L2
    else if L2 = [] then L1
    else 
        if member(hd(L1),L2) = true then union(tl(L1),L2)
        else hd(L1)::union(tl(L1),L2);
        
fun product(x,L) = 			
    if L = [] then []
    else (x,hd(L))::product(x,tl(L));

fun cartesian_product(L1,L2) = 
    if L1 = [] then []
    else
        union(product(hd(L1),L2), cartesian_product(tl(L1),L2));


fun max_helper(x,[]) = x
| max_helper(x,y::L) = 
    if y>x then max_helper(y,L)
    else max_helper(x,L);
    
fun max(L) = 
    if L = [] then ~1 			(*negative numbers are denoted with ~*)
    else max_helper(hd(L),L);

fun remove(x,L) = 
    if L = [] then []
    else if x = hd(L) then remove(x,tl(L))
    else hd(L)::remove(x,tl(L));


fun find_biggest_two(L) = 		(*find biggest using max function and then remove*)
    let val biggest = max(L)		(*all occurrences of the max number and call max again*)
    in
        if L = [] then (~1,~1)
        else (biggest,max(remove(biggest,L)) )
    end;


fun find_val_helper(max,min,L) = 			(*helper function for find max and min*)
    if L = [] then (max,min)
    else if hd(L) > max then find_val_helper(hd(L),min,tl(L))
    else if hd(L) < min then find_val_helper(max,hd(L),tl(L))
    else find_val_helper(max,min,tl(L));

fun find_biggest_and_smallest(L) = 
    if L = [] then (~1,~1)
    else if tl(L) = [] then (hd(L),hd(L))
    else
        find_val_helper(hd(L),hd(L),tl(L));

fun remove_duplicates(L) = 			(*remove all duplicates in a list*)
    if L = [] then []
    else hd(L)::remove_duplicates(remove(hd(L),tl(L)));

fun sumpairs(L) = 				(*sum every 2 numbers*)	
    if L = [] orelse tl(L) = [] then L
    else 
        hd(L)+hd(tl(L)) :: sumpairs(tl(tl(L)));


fun dec_to_nbit_bin(x,n) = 
    if n = 0 then []
    else if x>=n then 1::dec_to_nbit_bin(x -n, n div 2)
    else 0::dec_to_nbit_bin(x, n div 2);


fun convert(x,n) = dec_to_nbit_bin(x,n); (*8 bit can go up to 128*)

fun bit_val(x) = (*returns 2^x*)
    if x = 1 then 1
    else 2*bit_val(x-1);
    
fun or(bin1,bin2) = 
    if bin1 = [] orelse bin2 = [] then []
    else if hd(bin1) = 1 orelse hd(bin2) = 1
        then 1::or(tl(bin1),tl(bin2))
    else 0::or(tl(bin1),tl(bin2));
    
fun andd(bin1,bin2) = 
    if bin1 = [] then []
    else if hd(bin1) + hd(bin2) = 2 
        then 1::andd(tl(bin1),tl(bin2))
    else 0::andd(tl(bin1),tl(bin2));

fun xor(bin1,bin2) = 
    if bin1 = [] then []
    else if hd(bin1) = hd(bin2) 
        then 0::xor(tl(bin1),tl(bin2))
    else 1::andd(tl(bin1),tl(bin2));

(*currently converts decimals to 8-bit decimals
  Function also supports other number of bits, just change bit_val value
  Functions can be modified to support various other bitwise operations*)

fun bitwise_or(x,y) = or(convert(x,bit_val(8)),convert(y,bit_val(8)));
fun bitwise_and(x,y) = andd(convert(x,bit_val(8)),convert(y,bit_val(8)));
fun bitwise_xor(x,y) = xor(convert(x,bit_val(8)),convert(y,bit_val(8)));


fun indivisible(x,y) = 		(*iterates y from 2 to x/2 to see if x mod y >0*)
    if x div 2 = y then true
    else if x mod y = 0 then false
    else indivisible(x,y+1);
    
fun isprime(x) = 		(*check if a number is prime*)
    if x < 2 then true
    else indivisible(x,2);


(*__________________________________________________________________*)
(*Some test cases:*)
(*val x = [1,2,3,4,5,6];
val y = [2,3,4,5,6,7];
nth(x,5);
take(x,3);
drop(x,3);
val zipped = zip(x,y);
val unzipped = unzip(zipped);
range(1,6);
rev(x);
last_n(x,2);
badd([0,1,1,1,1],[1,1,1,1]);
val merged = merge_sort([4,2,7,5,6,4]);
val merged1 = merge_sort([1,2,3,4,5,2]);
merge_helper([9,2,3],[3,4,5]);
val list = ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"];
val packer = pack(list);   
val encoded = encode(list);
encode1(list);
decode(encoded);
val list1 = [2,3,5];
val list2 = [2,3,5];
cartesian_product(list1,list2);
max([~1000,1,20,31]);*)
bitwise_or(28,56);
bitwise_and(128,128);
bitwise_xor(128,128);
isprime(47);

(*__________________________________________________________________*)



(*How to declare new datatypes and initialize values of a specific datatype*)

datatype elements = num of int | char of string;

fun count(L:''elements list) = 
    if L = [] then 0
    else 1+ count(tl(L));

val m = num(1);
val l = char("2");
count([m,l]);

