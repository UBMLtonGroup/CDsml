

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

 end

structure currentFrame = 
struct 
  datatype T =  Frame;
  val lengths : int array = Array.array(RAWFRAME_MAX_PLANES,0);
  val callsigns = Array.array(RAWFRAME_MAX_SIGNS,"a");
  val Positions : Pos.t array =
    Array.array(RAWFRAME_MAX_PLANES,(0.0,0.0,0.0));
  val planecnt : int ref  = ref 0; 
end


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
 



(*Call signs are indexed per plane in the array. i.e. callsign at index i is the
* callsign of plane i
* If the callsign is not present in statetable, then the motion is same as
* current Position. If callsign is present motion is vector from old Position to
* new Position*)
fun TRANSIENTDETECTOR_createMotions (i) = 
        let 
          val p :Pos.t = Array.sub(currentFrame.Positions, i)
          val c  = Array.sub(currentFrame.callsigns, i)

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


fun TRANSIENTDETECTOR_run() = 
 let
   val motions : Motion.t list = []
 in
   for (1 to RAWFRAME_MAX_PLANES) (fn i => TRANSIENTDETECTOR_createMotions(i)
   :: motions)
 end;



structure Reducer =
struct 

    val voxel_size = 10.0;
    val proximity_radius = 1.0;


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




end


val _ = print ("done\n")
