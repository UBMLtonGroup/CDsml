

datatype for = to of int * int
             | downto of int * int

infix to downto

val for =
    fn lo to up =>
       (fn f => let fun loop lo = if lo > up then ()
                                  else (f lo; loop (lo+1))
                in loop lo end)
     | up downto lo =>
       (fn f => let fun loop up = if up < lo then ()
                                  else (f up; loop (up-1))
                in loop up end)





(*structure HashMap = 
struct 
  type ('a, 'b) hashmap = ('a * 'b ref) list

  fun create ()= [] : ('a, 'b) hashmap

  fun insert(key,value) = 


end;
*)


structure Constants =
struct 
  val proximity_radius = 1.0
  val MIN_X = 0.0;
  val MIN_Y = 0.0;
  val MAX_X = 1000.0;
  val MAX_Y = 1000.0;
  val MIN_Z = 0.0;
  val MAX_Z = 10.0;

end;


signature RawFrame = 
sig

val length : int -> 'a array
val callsigns : int -> 'a array
val Positions : int -> 'a array
val planeCnt : int 
end;

val RAWFRAME_MAX_PLANES = 1000;
val RAWFRAME_MAX_SIGNS = 10 * RAWFRAME_MAX_PLANES;


structure Pos = 
struct
  type t = real*real*real

  val zero = (0.0,0.0,0.0)
  val errorcode = (0.001,0.001,0.001)
  
  (*Used for quality of reals as well*)
  fun nearlyEqual (a, b, eps) =
    let 
      val absA = Real.abs a
      val absB = Real.abs b
      val diff = Real.abs (a - b)
    in
      Real.== (a, b) orelse
     ( if Real.== (a, 0.0) orelse
          Real.== (b, 0.0) orelse
          diff < Real.minNormalPos
       then diff < eps * Real.minNormalPos
       else diff / Real.min (absA + absB, Real.maxFinite) < eps )
    end

  fun eq ((a:real,b:real,c:real):t, (d:real,e:real,f:real):t) =
  let
    val (x:real ,y:real ,z :real)= (a,b,c) 
    val (w:real,r:real,s:real)=(d,e,f)
  in
    if (nearlyEqual(a,d,0.1) andalso nearlyEqual(b,e,0.1) andalso
    nearlyEqual(c,f,0.1)) then true else false
  end



  fun getx(x,y,z)=x
  fun gety(x,y,z)=y
  fun getz(x,y,z)=z

  fun getString(p :t)=
    let
        val (a,b,c) = p
    in
        "(" ^ Real.toString(a) ^","^ Real.toString(b) ^","^ Real.toString(c) ^")"
    end

 end


structure Frames =
struct 


 type t =  (int * string array * Pos.t array) option;
  (*val lengths : int array = Array.array(RAWFRAME_MAX_PLANES,0);
  val callsigns = Array.array(RAWFRAME_MAX_SIGNS,"a");
  val positions : Pos.t array =
    Array.array(RAWFRAME_MAX_PLANES,(0.0,0.0,0.0));
  val planecnt : int ref  = ref 0; *)

  val empty :t  = NONE


  exception EmptyFrame;

  fun getCallsigns(frame:t) =
  let
    val (n,cs,pos) = Option.valOf(frame)
  in
    if Option.isSome(frame) then cs else raise EmptyFrame
  end

  fun getPosition(frame:t)=
  let 
    val (n,cs,pos) = Option.valOf(frame)
  in
    if Option.isSome(frame) then pos else raise EmptyFrame 
  end

  fun getPlaneCnt(frame:t) = 
  let
    val (n,cs,pos) = Option.valOf(frame)
  in 
    if Option.isSome(frame) then n else raise EmptyFrame 
  end

 
  open TextIO

 fun arrayToList arr = Array.foldr (op ::) [] arr
 fun printList (xs, sep, f) = print (String.concatWith sep (map f xs))
 fun printFrame (nameArr, posArr) = 
     let
         val nameLs = arrayToList nameArr
         val posLs  = arrayToList posArr
          
     in
         for (1 to (Array.length(nameArr)-1)) (fn i => print(Array.sub(nameArr,i)^" "^Pos.getString(Array.sub(posArr,i)) ^ "\n") )

     end



  exception InvalidNumber;
  
  fun readFromFile (f) =
  let
    val filestream = openIn(f)


    fun readFrame (stream) = 
        let
            fun revLsToArr x = Array.fromList(List.rev x)  

            fun convertTuple(l) = 
              let
                val one :: xs = l
                val two::ys = xs
                val three :: zs = ys 
              in
                (*print(Real.toString(one)^"\n");
                print(Real.toString(two)^"\n");
                print(Real.toString(three)^"\n");*)
                if not (List.null(zs)) then Pos.errorcode else (one,two,three)
              end 


            fun processLine line =
                let val (name::(rest)) = String.tokens (fn c => c = #" ") line
                in
                (name, convertTuple (map (fn n => case Real.fromString n of
                                 SOME number => number
                               | NONE => raise InvalidNumber)
                   rest))
                end
            fun readlines (firstLs, secondLs) =
                case TextIO.inputLine(stream) of
                    SOME line => if String.isPrefix("Frame")(line) then 
                          readlines (firstLs, secondLs)
                        else if String.size(line) > 1 then 
                          let 
                            val (name, pos) = processLine(line) 
                          in
                            readlines ((name::firstLs), (pos::secondLs))
                          end
                        else
                          (firstLs, secondLs)

                    | NONE      => (firstLs, secondLs)
            val (nameLs, posLs) = readlines([], [])



        in
           (revLsToArr nameLs, revLsToArr posLs)
        end




    
  in
     readFrame(filestream)
  end


  (*structure currentFrame = 
    struct 
  datatype T =  Frame;
  (*val lengths : int array = Array.array(RAWFRAME_MAX_PLANES,0);*)
  val callsigns = Array.array(RAWFRAME_MAX_SIGNS,"a");
  val positions : Pos.t array =
    Array.array(RAWFRAME_MAX_PLANES,(0.0,0.0,0.0));
  val planecnt : int ref  = ref 0; 
end
*)

  fun createFrameBuffer(f)=
  let
    val (cs,pos) = readFromFile(f)

    (*getting num of planes from num of callsigns instead of frames file*)
    val nplanes = Array.length(cs)

    val frameBuffer :t list = []


  in
    SOME (nplanes,cs,pos) :: frameBuffer
  end  

  

end

(*structure CurrentFrame = 
struct 
  (*val lengths : int array = Array.array(RAWFRAME_MAX_PLANES,0);
  val callsigns = Array.array(RAWFRAME_MAX_SIGNS,"a");
  val positions : Pos.t array =
    Array.array(RAWFRAME_MAX_PLANES,(0.0,0.0,0.0));
  val planecnt : int ref  = ref 0; *)

end*)


structure Motion = 
struct 
  type t = (string * Pos.t * Pos.t);

  val motions : t list = [];
  val add = op ::

  fun getfirstPosition (m :t ) =
  let
    val (cs,Posone,Postwo) = m
  in
    Posone
  end

 fun getsecondPosition (m :t ) =
  let
    val (cs,Posone,Postwo) = m
  in
    Postwo
  end

  fun getAircraft(m :t) = 
  let 
    val (cs,_,_) = m
  in
    cs
  end

  (*returns the intersection of two motions if exists, returns Pos.errocode
  * otherwise *)
  fun findIntersection(one :t, two :t) = 
  let
    val i1 = getfirstPosition(one)
    val f1 = getsecondPosition(one)
    val i2 = getfirstPosition(two)
    val f2 = getsecondPosition(two)
    val r = Constants.proximity_radius
    val vx1 = Pos.getx(f1) - Pos.getx(i1)
    val vx2 = Pos.getx(f2) - Pos.getx(i2)
    val vy1 = Pos.gety(f1) - Pos.gety(i1)
    val vy2 = Pos.gety(f2) - Pos.gety(i2)
    val vz1 = Pos.getz(f1) - Pos.getz(i1)
    val vz2 = Pos.getz(f2) - Pos.getz(i2)
    (* this test is not geometrical 3-d intersection test, it takes the fact that the aircraft move
       into account ; so it is more like a 4d test
       (it assumes that both of the aircraft have a constant speed over the tested interval)
       we thus have two points, each of them moving on its line segment at constant speed ; we are looking
       for times when the distance between these two points is smaller than r 
       V1 is vector of aircraft 1
       V2 is vector of aircraft 2
       if a = 0 iff the planes are moving in parallel and have the same speed (can be zero - they may not be moving at all)
       a = (V2 - V1)^T * (V2 - V1) = < (V2 - V1), (V2 - V1) > =  sqrt( || V2 - V1 || )
    *)
    val a = (vx2 - vx1) * (vx2 - vx1) + (vy2 - vy1) * (vy2 - vy1) + (vz2 - vz1) * (vz2 - vz1);
    
    val b = 2.0 * (Pos.getx(i2) * vx2 - Pos.getx(i2) * vx1 - Pos.getx(i1) * vx2 + Pos.getx(i1) * vx1 + Pos.gety(i2) * vy2 - Pos.gety(i2) * vy1 - Pos.gety(i1) * vy2 + Pos.gety(i1) * vy1 + 
            Pos.getz(i2) * vz2 - Pos.getz(i2) * vz1 - Pos.getz(i1) * vz2 + Pos.getz(i1) * vz1);

    val c = ~r * r + (Pos.getx(i2) - Pos.getx(i1)) * (Pos.getx(i2) - Pos.getx(i1)) + (Pos.gety(i2) - Pos.gety(i1)) * (Pos.gety(i2) - Pos.gety(i1)) + (Pos.getz(i2) - Pos.getz(i1))* (Pos.getz(i2) - Pos.getz(i1));

    val discr = b*b - 4.0 *a *c

    (*the left side*)
    val v1 = ( ~b - Math.sqrt(discr) ) / (2.0 * a)
    (*the right side*)
    val v2 = (~b + Math.sqrt(discr)) / (2.0 * a)

    val x1col = Pos.getx(i1) + vx1 * (v1 + v2) / 2.0;
    val y1col = Pos.gety(i1) + vy1 * (v1 + v2) / 2.0;
    val z1col = Pos.getz(i1) + vz1 * (v1 + v2) / 2.0;
    
    val dist = Math.sqrt((Pos.getx(i2) - Pos.getx(i1)) * (Pos.getx(i2) - Pos.getx(i1)) + (Pos.gety(i2) - Pos.gety(i1)) * (Pos.gety(i2) - Pos.gety(i1)) + (Pos.getz(i2) - Pos.getz(i1)) * (Pos.getz(i2) - Pos.getz(i1)))

  in
    if not (Real.compare(a,0.0) = EQUAL) then
      (* we are first looking for instances of time when the planes are exactly r from each other
	 at least one plane is moving ; if the planes are moving in parallel, they do not have constant speed
	 if the planes are moving in parallel, then
	     if the faster starts behind the slower, we can have 2, 1, or 0 solutions
	     if the faster plane starts in front of the slower, we can have 0 or 1 solutions
	
         if the planes are not moving in parallel, then
	
         point P1 = I1 + vV1
	 point P2 = I2 + vV2
	   - looking for v, such that dist(P1,P2) = || P1 - P2 || = r
			
	 it follows that || P1 - P2 || = sqrt( < P1-P2, P1-P2 > )
	   0 = -r^2 + < P1 - P2, P1 - P2 >
	  from properties of dot product
	   0 = -r^2 + <I1-I2,I1-I2> + v * 2<I1-I2, V1-V2> + v^2 *<V1-V2,V1-V2>
	   so we calculate a, b, c - and solve the quadratic equation
	   0 = c + bv + av^2
	  
           b = 2 * <I1-I2, V1-V2>
           
           c = -r^2 + (I2 - I1)^T * (I2 - I1)
      *
      * *)
      ( if discr < 0.0 then Pos.errorcode else 
        ( if (v1 <= v2 andalso (v1 <= 1.0 andalso 1.0 <= v2 orelse v1 <= 0.0 andalso 0.0 <= v2 orelse 0.0 <= v1 andalso v2 <= 1.0)) then
            (* calculate the location of the collision; if it is outside of the bounds of the Simulation, don't do anything! *)
            ( if (z1col > Constants.MIN_Z andalso z1col <= Constants.MAX_Z
            andalso x1col >= Constants.MIN_X andalso x1col <= Constants.MAX_X
            andalso y1col >= Constants.MIN_Y andalso y1col <= Constants.MAX_Y)
              then (x1col,y1col,z1col) else Pos.errorcode )
          else
            Pos.errorcode 
        )
      )
    else (* a = 0.0*)
        (
            (* the planes have the same speeds and are moving in parallel (or they are not moving at all)
	       they  thus have the same distance all the time ; we calculate it from the initial point
	       dist = || i2 - i1 || = sqrt(  ( i2 - i1 )^T * ( i2 - i1 ) )
            *)

            if dist <=r then getfirstPosition(one) else Pos.errorcode 

        
        )    

  end 


end

structure StateTable  = 
struct 

  type t = string * Pos.t ref;

  val table : t list = [];



  fun search (cs,[]) = ref Pos.errorcode
    | search (cs,(x,Posval) ::tl) = 
        if String.compare(x,cs) = EQUAL  then Posval
        else search (cs,tl)   
  
 (* puts a Position in the table. if new, creates a new entry to list, if
 * callsign already exists, updates the Position*)
  fun put (cs , Position :Pos.t) = 
    let
      val p = search(cs,table)
    in
      if Pos.eq(!p,Pos.errorcode) then ((cs,ref Position) :: table; ())
      else p:= Position
    end
  
(*gets the corresponding Position from table, given the callsign*)
  fun get (cs) = 
    let
      val x = search (cs,table)
    in
      !x
    end;

end


structure Voxel=
struct 
  type t = (real*real)

  fun nearlyEqual (a, b, eps) =
    let 
      val absA = Real.abs a
      val absB = Real.abs b
      val diff = Real.abs (a - b)
    in
      Real.== (a, b) orelse
     ( if Real.== (a, 0.0) orelse
          Real.== (b, 0.0) orelse
          diff < Real.minNormalPos
       then diff < eps * Real.minNormalPos
       else diff / Real.min (absA + absB, Real.maxFinite) < eps )
    end


 fun eq ((a:real,b:real), (d:real,e:real)) =
  let
    val (x:real ,y:real)= (a,b) 
    val (w:real,r:real)=(d,e)
  in
    if (nearlyEqual(a,d,0.01) andalso nearlyEqual(b,e,0.01)) then true else false
  end

end


structure Collision = 
struct 
  (*aircraft one, aircraft two, position where collision happened*)
  type t = string * string  * Pos.t

  fun create (a1,a2,intersection:Pos.t) = (a1,a2,intersection)

  fun empty () :t list = []

  fun getLocation (c :t) = 
  let
    val (one,two,pos) = c
  in
    pos
  end

  fun getAircraftOne(c :t) = 
  let
    val (one,two,pos) =c
  in
    one
  end

  fun getAircraftTwo(c :t) = 
  let
    val (one,two,pos) = c
  in 
    two
  end

end 
 
structure VoxelMap  = 
struct 

  type t = Voxel.t * Motion.t list ref;

  fun makemap () : t list = [];


  fun getmotions (v,[]) = ref []
    | getmotions(v,(voxel,ml):: tl) = 
    if Voxel.eq(v,voxel) then ml else getmotions(v,tl)
     


(*Takes a voxel as key, motion and Map, adds motion to list of motions in Map
* indexed at voxel
* returns updated map*)
  fun put(v:Voxel.t, m:Motion.t,map:t list) = 
  let
    val motionlist = getmotions(v,map)
    val ml = !motionlist
  in
    if List.null(ml) then ((v, ref [m]) :: map ) else (motionlist := (m :: ml); map)
  end 


  fun retmotion ([]) = []
    |retmotion ((voxel,ml) :: tl) = (ml; retmotion(tl))

  fun getv([]) = []
    | getv((voxel,ml) ::tl) =  !ml :: getv(tl) 

  
  fun getvalues(map) =  getv (!map)
 
end






structure Reducer =
struct 


    
    val voxel_size = 10.0;
    val proximity_radius = Constants.proximity_radius;


    val horizontal = (voxel_size,0.0);
    val vertical = (0.0,voxel_size);

    (*create a 2d point representing a voxel*)
    fun voxelHash(Position :Pos.t) = 
    let
      val (x,y,_) = Position

      val voxelx = voxel_size * (x/voxel_size)
      val voxely = voxel_size * (y/voxel_size)

    in
      (if x < 0.0 then voxelx-voxel_size else voxelx, if y < 0.0 then
        voxely-voxel_size else voxel_size)
    end


  (*given voxel and motion, determine if motion is in voxel*)
  fun isInVoxel(voxel,motion :Motion.t) =
  let
    open Pos
    val(voxelx,voxely) = voxel
    val init = Motion.getfirstPosition(motion)
    val fin = Motion.getsecondPosition(motion)
    val v_s = voxel_size
    val r = proximity_radius /2.0
    
    val v_x =  voxelx
    val x0 = getx(init)
    val xv = getx(fin) - getx(init)

    val v_y = voxely
    val y0 =  gety(init)
    val yv = gety(fin)- gety(init)

    val low_x : real ref = ref ((v_x - r - x0)/xv)
    val high_x :real ref = ref ((v_y + v_s + r - y0)/yv)
     
    val low_y : real ref = ref((v_y-r-y0)/yv)
    val high_y : real ref = ref((v_y+v_s+r-y0)/yv)

    fun swap (a,b) = 
    let 
      val tmp : real ref = ref 0.0
    in
      tmp := !a;
      a := !b;
      b:= !tmp
    end;

    fun getResult () = 
      if (
        (
            ( Pos.nearlyEqual(xv,0.0,0.01) andalso v_x <= x0 + r andalso x0 - r <= v_x + v_s) (* no motion in x *) orelse 
                    ((!low_x <= 1.0 andalso 1.0 <= !high_x) orelse (!low_x <= 0.0 andalso 0.0 <= !high_x) orelse (0.0 <= !low_x andalso !high_x <= 1.0))
        )
        
        andalso 
       
        (
            ( (Pos.nearlyEqual(yv,0.0,0.01)) andalso (v_y <= y0 + r) andalso (y0 - r <= v_y + v_s ) ) (* no motion in y *) orelse 
                    ( (!low_y <= 1.0)  andalso (1.0 <= !high_y) )  orelse ( (!low_y
                    <= 0.0) andalso (0.0 <= !high_y) ) orelse ( (0.0 <= !low_y)
                    andalso (!high_y <= 1.0))
        )
        
        andalso

        ( Pos.nearlyEqual(xv,0.0,0.01) orelse (Pos.nearlyEqual(yv,0.0,0.01)) orelse (* no motion in x or y or both *)
                    (!low_y <= !high_x andalso !high_x <= !high_y) orelse
                    (!low_y <= !low_x andalso !low_x <= !high_y) orelse (!low_x <= !low_y andalso !high_y <= !high_x))
        ) then true else false

  in
    (if xv<0.0 then swap(low_x,high_x) else ();
    if yv< 0.0 then swap(low_y,high_y) else ();
    getResult ()
        
    )
  end; 


  (*Returns a list of motions which have possible collisions. Unlike in CDj
  * which returns a Hashmap and then extract values from it in reduce collision.*)
  fun dfsVoxelHashRecurse(motion :Motion.t,next_voxel,voxel_map,graph_colors) = 
  let
    val tmp :(real*real) ref  = ref (0.0,0.0)

    val (nvx,nvy) = next_voxel
    val (hx,hy)= horizontal
    val (vx,vy) = vertical
    
    (*Left Boundary*)
    val lb = (nvx-hx,nvy-hy)

    (*Right Boundary*)
    val rb = (nvx+hx,nvy+hy)

    (*Upper Boundary*)
    val ub = (nvx+vx,nvy+vy)

    (*Lower Boundary*)
    val lob = (nvx-vx,nvy-vy)

    (*Upper-Left*)
    val (ulx,uly)= (nvx-hx,nvy-hy)
    val ul = (ulx+vx,uly+vy)

    (*Upper-Right*)
    val (urx,ury) = (nvx+hx,nvy+hy)
    val ur = (urx+vx,ury+vy)

    (*Lower-Left*)
    val (llx,lly) = (nvx-hx,nvy-hy)
    val ll = (llx-vx,lly-vy)

    (*Lower-Right*)
    val (lrx,lry) = (nvx+hx,nvy+hy)
    val lr = (lrx-vx,lry-vy)


  in
    if isInVoxel(next_voxel,motion) andalso true then 
      (
        next_voxel :: graph_colors;
        voxel_map := VoxelMap.put(next_voxel,motion,!voxel_map);

        dfsVoxelHashRecurse(motion,lb,voxel_map,graph_colors);
        dfsVoxelHashRecurse(motion,rb,voxel_map,graph_colors);
        dfsVoxelHashRecurse(motion,ub,voxel_map,graph_colors);
        dfsVoxelHashRecurse(motion,lob,voxel_map,graph_colors);
        dfsVoxelHashRecurse(motion,ul,voxel_map,graph_colors);
        dfsVoxelHashRecurse(motion,ur,voxel_map,graph_colors);
        dfsVoxelHashRecurse(motion,ll,voxel_map,graph_colors);
        dfsVoxelHashRecurse(motion,lr,voxel_map,graph_colors)

       ) else (voxel_map,graph_colors)
  end;



    fun performVoxelHashing(motion,voxel_map,graph_colors) = 
    let
      val voxel = voxelHash(Motion.getfirstPosition(motion))
    in
      dfsVoxelHashRecurse(motion,voxel,voxel_map,graph_colors)
    end;


    


  end;


(* returns list of list of motions*)
(*graph_colors need not be a ref because it has to be cleared in perform voxel
* hashing anyways*)
fun reduceCollisionSet(motions :Motion.t list) = 
let 
  val voxel_map = ref (VoxelMap.makemap())
  val graph_colors = []

  fun pvh (m) = 
    let 
        val (a,b) = Reducer.performVoxelHashing(m,voxel_map,graph_colors)
    in
        ()(*graph_colors := b @ !graph_colors*)
    end

  fun traverseListofMotions ([]) = ()
    | traverseListofMotions (h :: t) =  (pvh (h); traverseListofMotions(t))



in
   traverseListofMotions(motions);
   VoxelMap.getvalues(voxel_map)
end 


(*returns list of triples Aircraft1 * aircraft2 * coordinates of collision 
* Each list of motions corresponds to motions in a voxel that can possibly have
* collisions. there can be more than one collision in a voxel hence it returns
* list of collisions
* *)
fun determineCollisions(motions : Motion.t list) = 
let
  
  val ret = Collision.empty()

  fun determine ([]) = ret 
    | determine (one :: tl) = 
  let 
    val two = List.hd (tl )
    val vec = Motion.findIntersection(one,two)
  in
    if not (Pos.eq(vec,Pos.errorcode)) then
     (Collision.create(Motion.getAircraft(one),Motion.getAircraft(two),vec) ::
     ret;
     determine(tl))
    else
     determine(tl)
  end  

in
    determine(motions)
end 



fun lookforCollisions(motions : Motion.t list) = 
let
  val check = reduceCollisionSet(motions) (*List of list of motions*)

  val c = Collision.empty()

  fun map f [] = []
    | map f (x :: xs) = (f x @ map f xs )
in 
 c @ map determineCollisions check 
end





(*Call signs are indexed per plane in the array. i.e. callsign at index i is the
* callsign of plane i
* If the callsign is not present in statetable, then the motion is same as
* current Position. If callsign is present motion is vector from old Position to
* new Position*)
fun TCM (i,currentFrame) = 
        let 
          val p :Pos.t = Array.sub(Frames.getPosition(currentFrame), i)
          val c  = Array.sub(Frames.getCallsigns(currentFrame), i)

        fun createMotions (cs, Position :Pos.t) = 
            let 
                val old_Pos = StateTable.get(cs)
            in
              (StateTable.put(cs,Position);
              if Pos.eq(old_Pos,Pos.errorcode) then 
               (cs,Position,Position) else
                (cs,old_Pos,Position) )
            end

        in
        createMotions (c,p)
        end;


(*returns list of motions*)
fun TRANSIENTDETECTOR_createMotions(currentFrame) = 
let
   val mo : Motion.t list = []
 in
   for (1 to RAWFRAME_MAX_PLANES) (fn i => TCM(i,currentFrame)
   :: mo);
   mo
 end;




fun TRANSIENTDETECTOR_run(currentFrame) = 
 let
   val motions : Motion.t list = TRANSIENTDETECTOR_createMotions(currentFrame);

    val collisions = lookforCollisions(motions)

   fun printResults ([],index) = ()
     | printResults (c :: tl,index) = (print("CD Collision" ^ Int.toString(index) ^ "occured at location " ^ Pos.getString(Collision.getLocation(c)) ^
                                        " with aircraft " ^ Collision.getAircraftOne(c) ^ " and aircraft " ^
                                        Collision.getAircraftTwo(c) ^ "\n" );
                                       printResults(tl,index+1))
 in
   (print ("CD detected " ^ Int.toString(List.length(collisions)) ^ " collisions \n");
   printResults(collisions,0)
   )
 end;







structure Driver = 
struct


  (*To do read properly from file*)

    fun main() = 
    let 
      val frameBuffer = Frames.createFrameBuffer("frames.txt")
        
      fun loop ([]) = ()
        | loop(x::xs) =(TRANSIENTDETECTOR_run(x) ; loop xs)
    in
      loop frameBuffer
    end




end




val _ = Driver.main()


val _ = print ("done\n")
