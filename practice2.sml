
(*	The function takes consecutive pairs of values, adding them and inserting the sums into a new list.
	sumpairs([8, 2, 3, 1, 5, 4]) would return [10, 4, 9]
	sumpairs([8, 2, 3, 1, 5, 4, 7]) would return [10, 4, 9, 7]
 *)
fun sumpairs(L) = if (L = nil orelse tl(L) = nil) then L else hd(L)+List.nth(L,1)::skip(tl(L))and skip(L) = if (L=nil) then nil else sumpairs(tl(L));



(*function takes a list consisting of possible consecutive duplicate values and return a list of sublists of duplicates
	groupdupes([1,1,1,2,3,2,2]) would return [[1,1,1],[2],[3],[2,2]]*)

fun repeat(x,L) =
if (L=[]) then []
else 
    if (x=hd(L))
     then hd(L)::repeat(x,tl(L))
    else repeat(x,[]);


fun groupdupes(L) = 
if(L=[]) then [[]]
else
    [repeat(hd(L),L)]@skip(hd(L),tl(L))

and
 skip(x,L) =
 if (L=nil) then nil
 else 
    if(hd(L)=x)
        then skip(x,tl(L))
    else
        groupdupes(L);



fun isPrime(x,y):bool =
    if(x mod y = 0)
        then false
    else
        if(y * y >= x)
            then true
        else
            isPrime(x,y+1);

fun goldbachhelp(x,y) =
    if(x mod 2 = 1)
        then []
    else
        if(isPrime(y,2) andalso isPrime(x-y,2))
            then [x-y,y]
        else
            goldbachhelp(x,y+1);

fun goldbach(x) = goldbachhelp(x,2);


(*performs 8-bit bitwise and on two based-10 decimal numbers and returns a list of 	binary digits
	bitwise_and(31,12) would return [0,0,0,0,1,1,0,0]     *)
    
fun bitwise(x,y) = 
    if(y=0)
        then []
   else
     if(x - y >=0)
        then 1::bitwise(x-y,y div 2)
     else
        0::bitwise(x,y div 2);
        
fun bit8(x) = 
    bitwise(x,128);

fun bitwise_and_helper(L1,L2)=
    if(L1 = []) then L2
    else 
        if(L2 = []) then L1
        else
            if(hd(L1)=hd(L2))
                then hd(L1)::bitwise_and_helper(tl(L1),tl(L2))
            else
                0::bitwise_and_helper(tl(L1),tl(L2));
                
fun bitwise_and(L1,L2)=
    bitwise_and_helper(bit8(L1),bit8(L2));

