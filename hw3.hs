module HW3 where
import Data.List
			

--tuples prey(bos list)
ayirp [] prey = [] 
ayirp (x:xs) prey =
					if (length xs)/=0 then
										   if (ayir2 x)=='p' then ayirp xs (prey ++ [x]) 
										   else ayirp xs prey 
					else if ayir2 x == 'p' then prey ++ [x]
					else sira(prey)

--ayirh ::[(Char,Integer,Integer)]->[(Char,Integer,Integer,Float)]->Float->[(Char,Integer,Integer,Float)]
ayirh [] prey energy = sira1(prey)
ayirh (x:xs) prey energy =
							 if ayir2 x == 'h' then ayirh xs (prey ++ [ekle x energy]) energy
													   else ayirh xs prey energy
								
--tuples engel(bos list)
ayire [] prey = [] 
ayire (x:xs) prey =
								if (length xs)/=0 then if ayir2 x == 'x' then ayire xs (prey ++ [x])
													   else ayire xs prey
								else if ayir2 x == 'x' then prey ++ [x]
								else sira(prey)								
								
								
--ayir2 :: (Char,Integer,Integer)->Char								
ayir2 (x,_,_) =
				x
				
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

ayir3 a (_,y,z) =
				 if a==(y,z) then True
				 else False
--engellist,agent
engelvarmi [] a = True				 
engelvarmi (x:xs) (k,b,c) =
							if (length xs)/=0 then if ayir3 (b,c) x then False
												   else engelvarmi xs (k,b,c)
							else if ayir3 (b,c) x then False
												   else True
					

--tuple one cell,preys(tuple list),bos list
manhatdis1 (a,b,c) (d,e,f) = if b>e && c>f then b-e+c-f
							 else if b>e && c<f then b-e-c+f
							 else if b<e && c<f then -b+e-c+f
							 else -b+e+c-f
manhatdis ('x',b,c) (x:xs) a = [999]
manhatdis (a,b,c) [] k = [999]
manhatdis (a,-1,c) y k = [999]
manhatdis (a,15,c) y k = [999]
manhatdis (a,b,-1) y k = [999]
manhatdis (a,b,15) y k = [999]
manhatdis (a,b,c) (x:xs) k =
							if (length xs)/=0 then manhatdis (a,b,c) xs (k++ [manhatdis1 (a,b,c) x])
							else k++[manhatdis1 (a,b,c) x]
same a=
		minimum (a)
--tuple hunter,preys,engeller,boslist,int defaultta 1
--calcman :: (Char,Integer,Integer)->[(Char,Integer,Integer)]->[(Char,Integer,Integer)]->[Integer]->Integer->[Integer]									
calcman (a,b,c) prey engel k int=
							     if int==1 then if engelvarmi engel (a,b-1,c)==False then calcman (a,b,c) prey engel (k ++ [999]) 2
								                else calcman (a,b,c) prey engel (k ++ [same(manhatdis (a,b-1,c) prey [])]) 2
								 else if int==2 then if engelvarmi engel (a,b,c-1)==False then calcman (a,b,c) prey engel (k ++ [999]) 3
								                else calcman (a,b,c) prey engel (k ++ [same(manhatdis (a,b,c-1) prey [])]) 3
								 else if int==3 then if engelvarmi engel (a,b,c+1)==False then calcman (a,b,c) prey engel (k ++ [999]) 4
												else calcman (a,b,c) prey engel (k ++ [same(manhatdis (a,b,c+1) prey [])]) 4
								else if int==4  then if engelvarmi engel (a,b+1,c)==False then calcman (a,b,c) prey engel (k ++ [999]) 5
												else calcman (a,b,c) prey engel (k ++ [same(manhatdis (a,b+1,c) prey [])]) 5
	                            else k				
--tuple hunter,preys,engeller
movehunt (a,b,c) p e =
					  if same(calcman (a,b,c) p e [] 1)==(calcman (a,b,c) p e [] 1 !! 0) then (a,b-1,c)
					  else if same(calcman (a,b,c) p e [] 1)==(calcman (a,b,c) p e [] 1 !! 1) then (a,b,c-1)
					  else if same(calcman (a,b,c) p e [] 1)==(calcman (a,b,c) p e [] 1 !! 2) then (a,b,c+1)
					  else (a,b+1,c)
--prey(tuple) step(sayi) engel tuple list prey list
preystep (x,y,z) n engel a= 
							if fib n `mod` 5 == 0 then (x,y,z)
							else if fib n `mod` 5 == 1 then if (engelvarmi engel (x,y,(z-1)) && z-1>=0) && ((engelvarmi a (x,y,(z-1))==False && z-1==0)==False ) && ((engelvarmi a (x,y,(z-1))==False && engelvarmi engel (x,y,(z-2))==False)==False ) then (x,y,(z-1))
															else (x,y,z)
							else if fib n `mod` 5 == 2 then if (engelvarmi engel (x,y,(z+1)) && z+1<=14)&&((engelvarmi a (x,y,(z+1))==False && z+1==14)==False ) && ((engelvarmi a (x,y,(z+1))==False && engelvarmi engel (x,y,(z+2))==False)==False ) then (x,y,(z+1))
															else (x,y,z)
							else if fib n `mod` 5 == 3 then if (engelvarmi engel (x,(y-1),z) && y-1>=0)&&((engelvarmi a (x,y-1,z)==False && y-1==0)==False ) && ((engelvarmi a (x,y-1,(z))==False && engelvarmi engel (x,y-2,(z))==False)==False )  then (x,(y-1),z)
															else (x,y,z)
							else if (engelvarmi engel (x,(y+1),z) && y+1<=14) &&((engelvarmi a (x,y+1,(z))==False && y+1==14)==False ) && ((engelvarmi a (x,y+1,(z))==False && engelvarmi engel (x,y+2,(z))==False)==False ) then (x,(y+1),z)
							else (x,y,z)
capture (a,b,c,d) =
					(a,b,c,d+3.0)
biten (a,b,c,d) =
				 if d<=0 then False
				 else True
--hunters bos
bitenbul [] bos = sira1(bos)
bitenbul (x:xs) bos = if biten(x)==True then bitenbul xs (bos++[x])
					  else bitenbul xs bos
--hunters preys bos
--bulhunter :: [(Char,Integer,Integer,Float)]->[(Char,Integer,Integer)]->[(Char,Integer,Integer,Float)]->[(Char,Integer,Integer,Float)]
bulhunter [] prey bos = sira1(bos)
bulhunter a [] bos = a							
bulhunter (x:xs) prey bos=
							if engelvarmi prey (cikar x) then bulhunter xs prey (bos++[x])
							else bulhunter xs prey (bos++[capture x])

--preys hunters bos
bulprey	[] hunters bos =sira(bos)
bulprey a [] bos = a
bulprey (x:xs) hunters bos=
							if engelvarmi (cikarel hunters []) x then bulprey xs hunters (bos++[x])
							else bulprey xs hunters bos
--list tuple preys yeni,list tuple preys,engels,bos list,step,hunters,integer 1,kalan

movepreys [] preys engels bos n hunters y kalan= if (length bos)==0 then print_game_table (preys++engels++(cikarel hunters []))else
												 movehunts hunters preys engels [] (sira(bos)) n kalan hunters
movepreys (x:xs) preys engels bos n hunters y kalan=
							if y==1 then movepreys (bulprey preys hunters []) (bulprey preys hunters []) engels [] (n+1) (bitenbul (bulhunter hunters preys []) []) 0 (kalan-1)
							
							else if ((length preys)==0 || (length hunters)==0 || kalan==0)==True then print_game_table (preys++engels++(cikarel hunters []))
							 else if (engelvarmi bos (preystep x n engels preys))==True then movepreys xs preys engels (bos ++ [preystep x n engels preys]) n hunters 0 kalan
							else (movepreys xs preys engels (bos ++ [x]) n hunters 0 kalan)
								
--list tuple hunters,preys,engeller,bos list,yeni prey,step,kalan
movehunts [] preys engels bos yeniprey n kalan sss = movepreys yeniprey yeniprey engels [] n (sira1(bos)) 1 kalan				  
movehunts (x:xs) preys engels bos yeniprey n kalan sss =if engelvarmi (cikarel sss []) (movehunt (cikar x) preys engels) then if ((finde x)-1)>=0 then movehunts xs preys engels (bos ++ [tameksi(ekle (movehunt (cikar x) preys engels) (finde x))]) yeniprey n kalan (bos ++ [tameksi(ekle (movehunt (cikar x) preys engels) (finde x))])
														                                                                                         else movehunts xs preys engels (bos) yeniprey n kalan (bos ++ [tameksi(ekle (movehunt (cikar x) preys engels) (finde x))])
												    else if ((finde x)-0.2)>=0 then movehunts xs preys engels (bos ++ [bucukeksi(x)]) yeniprey n kalan (bos ++ [bucukeksi(x)])
													else movehunts xs preys engels (bos) yeniprey n kalan (bos ++ [bucukeksi(x)])
								  
cikar (a,b,c,_) =
					(a,b,c)
--4lü hunt list bos list
cikarel ::[(Char,Integer,Integer,Float)]->[(Char,Integer,Integer)]-> [(Char,Integer,Integer)]					
cikarel [] bos = bos
cikarel (x:xs) bos = cikarel xs (bos++[cikar x])
bucukeksi (a,b,c,d)=
					(a,b,c,d-0.2)
tameksi (a,b,c,d) =
					(a,b,c,d-1)
--tuples hunter(bos list)
finde (a,b,c,d)=
				d
ekle (a,b,c) d =
				(a,b,c,d)

sortGT (_,a1, b1) (_,a2, b2)
  | a1 < a2 = GT
  | a1 > a2 = LT
  | a1 == a2 = compare b2 b1
sortGT a b = GT					
sira lst =					
           reverse (sortBy sortGT lst)  
sortGT1 (_,a1,b1,_) (_,a2,b2,_)
  | a1 < a2 = GT
  | a1 > a2 = LT
  | a1 == a2 = compare b2 b1
sortGT1 a b = GT					
sira1 lst =					
           reverse (sortBy sortGT1 lst)   
--bu cagırcagın [[[Char]]]
main2 (x:xs)=
			 if (length xs)/=0 then do {mainl x; putStr ("\n") ; main2 xs; }
			else mainl x
mainl (x:xs)=
		    if (length xs)/=0 then do { putStr ( x ++ " ") ; mainl xs }
			else putStr (x)		
--tuple(agent) tek row	('p',row 0,colon 3)	
--inler :: (Char,Integer,Integer)->[[Char]]->[[Char]]			
inler (x,y,z) a = 
				  if x=='p' then take (fromIntegral z) a ++ ["P"] ++ drop ((fromIntegral z) + 1) a
				  else if x=='h' then take (fromIntegral z) a ++ ["H"] ++ drop ((fromIntegral z) + 1) a 
				  else take (fromIntegral z) a ++ ["X"] ++ drop ((fromIntegral z) + 1) a
--tuple enviroment				  
--inler2 :: (Char,Integer,Integer)->[[[Char]]]->[[[Char]]]
inler2 (x,y,z) a = 	take (fromIntegral y) a ++ [inler (x,y,z) (a!!(fromIntegral y))]++drop ((fromIntegral y) + 1) a	
--tuple listesi enviroment
inler3 [] a = a 
inler3	(x:xs) y = if (length xs)/=0 then inler3 xs (inler2 x y)
				   else inler2 x y


print_game_table a = main2 (inler3 a [["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"]])


simulate_the_game a e n = 
							
						 movepreys (ayirp a []) (ayirp a []) (ayire a []) [] 1 (ayirh a [] e) 0 (n)


	  
