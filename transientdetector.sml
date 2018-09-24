

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

  fun createFrame(n :int,cs:string array,pos :Pos.t array) :t =
    SOME (n,cs,pos)
 
  open TextIO

 fun arrayToList arr = Array.foldr (op ::) [] arr
 fun printList (xs, sep, f) = print (String.concatWith sep (map f xs))
 
 fun printFrame (f :t) = 
     let
         val nplanes = getPlaneCnt(f)
         val posArr  = getPosition(f)
         val csArr = getCallsigns(f)
          
     in

         print("\n");
         for (0 to (nplanes-1)) (fn i => print(Array.sub(csArr,i)^" "^Pos.getString(Array.sub(posArr,i)) ^ "\n") )

     end

 

  exception InvalidNumber;


  val frameBuffer :t list ref= ref []
  
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

            (*reads one frame and returns rest of stream*)    
            fun readlines (firstLs, secondLs,strm) =
                case StreamIO.inputLine(strm) of
                    SOME (line,str) => if String.isPrefix("Frame")(line) then 
                          readlines (firstLs, secondLs,str)
                        else if String.isPrefix("End")(line) then 
                          (firstLs,secondLs,str)
                        else if String.size(line) > 1 then 
                          let 
                            val (name, pos) = processLine(line) 
                          in
                            readlines ((name::firstLs), (pos::secondLs),str)
                          end
                        else
                          (firstLs, secondLs,str)

                    | NONE      => (firstLs, secondLs,strm)
            val (nameLs, posLs,st) = readlines([], [],stream)



        in
           (revLsToArr nameLs, revLsToArr posLs,st)
        end

  
  (*Reads frames from file and populates framebuffer*)
  fun readFromFile (f) =
  let
    val filestream = TextIO.openIn(f)

    

        fun createFrameBuffer(fstream)=
            let
                val (cs,pos,remainderStr) = readFrame(fstream)

                (*getting num of planes from num of callsigns instead of frames file*)
                val nplanes = Array.length(cs)

                val frame = createFrame(nplanes,cs,pos) 
            in
                frameBuffer := !frameBuffer @ [frame];
                if not(StreamIO.endOfStream(remainderStr)) then createFrameBuffer(remainderStr) else !frameBuffer 
            end  


    
  in
     createFrameBuffer(getInstream filestream)
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

  fun printMotion(m :t) = 
  let
    val (cs,p1,p2) = m
  in
    print("Plane "^ cs ^ " going from " ^ Pos.getString(p1) ^ " to "^
    Pos.getString(p2) ^"\n")
  end

  fun printListOfMotions([] :t list) = print ("No motions in list\n")
    |printListOfMotions(h :: tl) = (printMotion(h); printListOfMotions(tl))



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

  val table : t list ref = ref [];



  fun search (cs,[]) = ref Pos.errorcode
    | search (cs,(x,Posval) ::tl) = 
        if String.compare(x,cs) = EQUAL  then Posval
        else search (cs,tl)   
  
 (* puts a Position in the table. if new, creates a new entry to list, if
 * callsign already exists, updates the Position*)
  fun put (cs , position :Pos.t) = 
    let
      val p = search(cs,!table)
    in
      if Pos.eq(!p,Pos.errorcode) then ( table := (cs,ref position) :: !table; ())
      else p:= position
    end
  
(*gets the corresponding Position from table, given the callsign*)
  fun get (cs) = 
    let
      val x = search (cs,!table)
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

 fun printVoxel(v :t) = 
  let
    val (x,y) = v
  in
    print("Voxel : x coordinate "^Real.toString(x)^" y coordinate "^Real.toString(y) ^"\n")
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
     
  fun getLength(v)= List.length(v)

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
    fun voxelHash(position :Pos.t) = 
    let
      val (x,y,_) = position

      val voxelx = voxel_size * (x/voxel_size)
      val voxely = voxel_size * (y/voxel_size)

    in
      (if x < 0.0 then voxelx-voxel_size else voxelx, if y < 0.0 then
        voxely-voxel_size else voxely)
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
    ((*Voxel.printVoxel(voxel);
    Motion.printMotion(motion);  *)
    
       if xv<0.0 then swap(low_x,high_x) else ();
    if yv< 0.0 then swap(low_y,high_y) else ();

 (*print(Bool.toString((
            ( Pos.nearlyEqual(xv,0.0,0.01) andalso v_x <= x0 + r andalso x0 - r <= v_x + v_s) (* no motion in x *) orelse 
                    ((!low_x <= 1.0 andalso 1.0 <= !high_x) orelse (!low_x <= 0.0 andalso 0.0 <= !high_x) orelse (0.0 <= !low_x andalso !high_x <= 1.0))
        )
) );
    
    print(Bool.toString((
            ( (Pos.nearlyEqual(yv,0.0,0.01)) andalso (v_y <= y0 + r) andalso (y0 - r <= v_y + v_s ) ) (* no motion in y *) orelse 
                    ( (!low_y <= 1.0)  andalso (1.0 <= !high_y) )  orelse ( (!low_y
                    <= 0.0) andalso (0.0 <= !high_y) ) orelse ( (0.0 <= !low_y)
                    andalso (!high_y <= 1.0))
        )));
    
    print(Bool.toString(( Pos.nearlyEqual(xv,0.0,0.01) orelse (Pos.nearlyEqual(yv,0.0,0.01)) orelse (* no motion in x or y or both *)
                    (!low_y <= !high_x andalso !high_x <= !high_y) orelse
                    (!low_y <= !low_x andalso !low_x <= !high_y) orelse (!low_x
                    <= !low_y andalso !high_y <= !high_x))));     *)   

    getResult ()
        
    )
  end; 


  fun containsKey(key,[]) = false
    |containsKey(key, h:: tl) = if Voxel.eq(key,h) then true else containsKey(key,tl)
    


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
    if isInVoxel(next_voxel,motion) andalso not (containsKey(next_voxel,!graph_colors))  then 
      (
        (*print("IN ADDING VOXEL\n");*)
        graph_colors :=  next_voxel :: !graph_colors;
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
(*graph_colors need not be cleared in perform voxelHashing as its called with a
* ref [] each time in this function*)
fun reduceCollisionSet(motions :Motion.t list) = 
let 
  val voxel_map = ref (VoxelMap.makemap())
  val graph_colors = ref []

  fun pvh (m) = 
    let 
        val (a,b) = Reducer.performVoxelHashing(m,voxel_map,graph_colors)
    in
        ( (*print ("graph colors length
        "^Int.toString(List.length(!graph_colors))^"\n");*)
        graph_colors := []) (*graph_colors := b @ !graph_colors*)
    end

  fun traverseListofMotions ([]) = ()
    | traverseListofMotions (h :: t) =  (pvh (h); traverseListofMotions(t))



in
   traverseListofMotions(motions);
(*   print("Voxel map length " ^
*   Int.toString(VoxelMap.getLength(!voxel_map))^"\n");*)
   VoxelMap.getvalues(voxel_map)
end 


(*returns list of triples Aircraft1 * aircraft2 * coordinates of collision 
* Each list of motions corresponds to motions in a voxel that can possibly have
* collisions. there can be more than one collision in a voxel hence it returns
* list of collisions
* *)
fun determineCollisions(motions : Motion.t list) = 
let
  
  val ret = ref (Collision.empty())


  fun det (one,[]) = !ret
    | det (one,two :: tl) =
    let
      val vec = Motion.findIntersection(one,two)
    in
      (*print("collision at "^Pos.getString(vec));*)
      if not(Pos.eq(vec,Pos.errorcode)) then
        (ret:= Collision.create(Motion.getAircraft(one),Motion.getAircraft(two),vec) :: !ret;
         det(one,tl) )
      else
        det(one,tl)
    end
    

  fun determine ([]) = !ret 
    | determine (one :: tl) = (det(one,tl) ; determine(tl))


in
    (*print("determine motions length = "^
    * Int.toString(List.length(motions))^"\n");*)
    determine(motions)
end 



fun lookforCollisions(motions : Motion.t list) = 
let
  val check = reduceCollisionSet(motions) (*List of list of motions*)

  val c = Collision.empty()

  fun map f [] = []
    | map f (x :: xs) = (f x @ map f xs )

 fun printcheck  [] = ()
    | printcheck (x :: xs) = print("Check list i length "^Int.toString(List.length(x))^"\n")

  val ls = map determineCollisions check 
in 
  (*print("check length " ^ Int.toString(List.length(check))^"\n");*)
  (*printcheck check;
  print ("Collision length "^ Int.toString(List.length(ls)) ^"\n");*)
 c @ ls
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

        fun createMotions (cs, position :Pos.t) = 
            let 
                val old_Pos = StateTable.get(cs)
            in
              (StateTable.put(cs,position);
              if Pos.eq(old_Pos,Pos.errorcode) then 
               (cs,position,position) else
                (cs,old_Pos,position) )
            end


        in
         createMotions(c,p)      
        end;


(*returns list of motions*)
fun TRANSIENTDETECTOR_createMotions(currentFrame) = 
let
   val mo : Motion.t list ref= ref []

 in
   for (0 to (Frames.getPlaneCnt(currentFrame)-1)) (fn i => mo :=  !mo @ [TCM(i,currentFrame)]);
   !mo
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
   (*(*Motion.printListOfMotions(motions);*)print ("CD detected " ^ Int.toString(List.length(collisions)) ^ " collisions \n");
   printResults(collisions,0)
   *)()
 end;







structure Driver = 
struct


  (*To do read properly from file*)


    fun sleepForPeriod(t) =
     OS.Process.sleep t 


    val periodint = 50
    val period = Time.fromMilliseconds periodint;  



        


    fun printBenchmarkResults([] : IntInf.int list,i: int) = print("Detector completed successfully\n")
      | printBenchmarkResults(x ::xs,i ) =
      let
        val cnt = i+1
      in 
        if (x >= Time.toMicroseconds(period)) then
          (print("Task "^ Int.toString(i)^" missed the deadline\n");
            printBenchmarkResults(xs,cnt ))
        else
          printBenchmarkResults(xs,cnt)
      end


    

    fun benchmarkCD ts tc tr  =
    let
      
     fun min_arr arr = Array.foldl IntInf.min (Array.sub(arr,0)) arr 
     fun max_arr arr = Array.foldl IntInf.max (Array.sub(arr,0)) arr

     fun avg_arr arr = IntInf.div( (Array.foldl (op +) (Int.toLarge(0)) arr),
       Int.toLarge(Array.length(arr)) )

      fun std_dev arr = 
      let
        val mean = avg_arr arr
        val newarr = (Array.modify (fn i => (i-mean)*(i-mean)) arr;arr);
        fun meanofsqdiff newarr = avg_arr newarr
      in
        Math.sqrt(Real.fromLargeInt(meanofsqdiff arr))
      end 



      val responseList =  Array.array(Array.length(tr),Time.toMicroseconds(Time.zeroTime))
      val compList =  Array.array(Array.length(tr),Time.toMicroseconds(Time.zeroTime))
      val jitList =  Array.array(Array.length(tr),Time.toMicroseconds(Time.zeroTime))

      fun calculateTimes (i) = 
        let 
            val cTime = (Array.sub(tc,i) - Array.sub(ts,i))
            val jTime = (Array.sub(ts,i) - Array.sub(tr,i))
            val rTime = (cTime + jTime)
        in
          (*print(IntInf.toString(cTime)^": comp time \n");
          print(IntInf.toString(jTime)^": jitter time \n");

          print(IntInf.toString(Array.sub(tr,i))^": ideal release time \n");
          print(IntInf.toString(rTime)^": respnse time \n");*)
          Array.update(responseList,i,rTime);
          Array.update(compList,i,cTime); 
          Array.update(jitList,i,jTime) 
        end 
        

    in
        for(0 to (Array.length(tr) -1)) (fn i => calculateTimes(i));
        (*for(0 to (Array.length(tr) -1)) (fn i =>
        * print(IntInf.toString(Array.sub(responseList,i))^"\n"))*)
        (*printBenchmarkResults (Array.foldr (op ::) [] responseList,0)*)

        print("Detector Completed\n");
        print("=============================================================\n");
        print("Max response time = "^IntInf.toString(IntInf.div((max_arr responseList),1000))^" ms \n");
        print("Min response time = "^IntInf.toString(IntInf.div(min_arr responseList,1000))^" ms\n");
        print("Avg response time = "^IntInf.toString(IntInf.div(avg_arr responseList,1000))^" ms\n");
        print("Std Dev response time = "^Real.toString((std_dev responseList)/1000.00)^" ms\n");
        print("=============================================================\n");
        print("Max Computation time = "^IntInf.toString(IntInf.div(max_arr compList,1000))^" ms\n");
        print("Min Computation time = "^IntInf.toString(IntInf.div(min_arr compList,1000))^" ms\n");
        print("Avg computation time = "^IntInf.toString(IntInf.div(avg_arr compList,1000))^" ms\n");
        print("Std Dev Computation time = "^Real.toString((std_dev compList)/1000.00)^" ms\n");
        print("=============================================================\n");
        print("Max Jitter time = "^IntInf.toString(max_arr jitList)^"\n");
        print("Min Jitter time = "^IntInf.toString(min_arr jitList)^"\n");
        print("Avg Jitter time = "^IntInf.toString(IntInf.div(avg_arr jitList,1000))^" ms\n");
        print("Std Dev Jitter time = "^Real.toString((std_dev jitList)/1000.00)^" ms\n")

    end;








    fun main() = 
    let 
      val frameBuffer = Frames.readFromFile("frames_col.txt")

      val maxFrames = 1000
      
      
      val ts = Array.array(maxFrames,Time.toMicroseconds (Time.zeroTime)) 
      val tc = Array.array(maxFrames,Time.toMicroseconds(Time.zeroTime) )
      
      val tr = Array.array(maxFrames,Time.toMicroseconds(Time.zeroTime) )

    fun populateIdealTime(t0) = 
        for(0 to (Array.length(tr)-1) ) (fn i => Array.update(tr,i, (t0 + (Time.toMicroseconds(period)* Int.toLarge(i))) ) ) 

           
      val responseTimeList : Time.time list ref  = ref [];
      
      fun loop ([],i) = ()
        | loop(x::xs,i) = if not(i = maxFrames) then (Array.update(ts,i,Time.toMicroseconds (Time.now()));
                                                        (*print(IntInf.toString(Array.sub(ts,i))^"\n"); *)
                                                        TRANSIENTDETECTOR_run(x) ; 
                                                        Array.update(tc,i,Time.toMicroseconds(Time.now ()) );
                                                        (*print(IntInf.toString(Time.toMicroseconds(Time.now()))^"\n");*)
                                                        sleepForPeriod(period);
                                                        loop(xs,(i+1)) ) 
                          else
                            ()
    in
      (*print (Int.toString(List.length(maxFrames)))*)
      loop(frameBuffer, 0);

      (*printArray(ts);*)
        
     populateIdealTime(Array.sub(ts,0));


      (benchmarkCD ts tc tr)

    end;




end




val _ = Driver.main()


val _ = print ("done\n")
