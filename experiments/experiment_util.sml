structure ExperimentUtil = struct
open Util
open MathUtil

val repetitions = 5

fun run command = let
		val results = map (fn _ => let
				val time_start = Time.toMilliseconds (Time.now ())
				val compiled_successfully = (OS.Process.isSuccess o OS.Process.system) command
				val time_end = Time.toMilliseconds (Time.now ())
			in
				(compiled_successfully, IntInf.toInt (time_end - time_start))
			end) (range 0 repetitions)
		val time = (reduce 0 plus (map (fn (_, t) => t) results)) div repetitions
		val compiled_successfully = (List.all id (map (fn (c, _) => c) results))
	in
		(compiled_successfully, time)
	end

fun time_mlton program = let
		val temp_file_ = OS.FileSys.tmpName ()
		val temp_file = temp_file_ ^ ".sml"
		val _ = OS.FileSys.rename {old=temp_file_, new=temp_file}
		val file_out = TextIO.openOut temp_file
		val _ = TextIO.output (file_out, program)
		val _ = TextIO.flushOut file_out
		val _ = TextIO.closeOut file_out
		val (compiled_successfully, run_time) = run ("mlton -stop tc " ^ temp_file ^ " 2> /dev/null")
		val _ = OS.FileSys.remove temp_file
		val _ = if not compiled_successfully then
				raise Fail ("Program did not compile:\n" ^ program)
			else ()
	in
		run_time
	end

fun time_elm program = let
		val current_dir = OS.FileSys.getDir ()
		val _ = if (OS.Path.file current_dir) <> "experiments" then
				raise Fail ("Run experiment from flunct/experiments")
			else ()
		val elm_sandbox = OS.Path.concat (current_dir, "elm_sandbox")
		val elm_file = OS.Path.concat (elm_sandbox, "temp.elm")
		val file_out = TextIO.openOut elm_file
		val _ = TextIO.output (file_out, program)
		val _ = TextIO.flushOut file_out
		val _ = TextIO.closeOut file_out
		val (compiled_successfully, run_time) = run ("cd elm_sandbox && elm make " ^ elm_file ^ " --output=/dev/null > /dev/null")
		val _ = (OS.Process.sleep o Time.fromSeconds) 1 (* wait for ELM compilation to finish *)
		val _ = OS.FileSys.remove elm_file
		val _ = if not compiled_successfully then
				raise Fail ("Program did not compile:\n" ^ program)
			else ()
	in
		run_time
	end

end
