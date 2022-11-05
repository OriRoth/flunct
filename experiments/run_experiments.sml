CM.make "experiments.cm";

(*
 * Ring: code size
 *)

(*
"Ring code size Shuffle";
RingExperiment.api_size Flunct.Shuffle 2;
RingExperiment.api_size Flunct.Shuffle 4;
RingExperiment.api_size Flunct.Shuffle 8;
RingExperiment.api_size Flunct.Shuffle 16;

"Ring code size Sparse";
RingExperiment.api_size Flunct.Sparse 2;
RingExperiment.api_size Flunct.Sparse 4;
RingExperiment.api_size Flunct.Sparse 8;
RingExperiment.api_size Flunct.Sparse 16;
RingExperiment.api_size Flunct.Sparse 32;
RingExperiment.api_size Flunct.Sparse 64;
RingExperiment.api_size Flunct.Sparse 128;
RingExperiment.api_size Flunct.Sparse 256;
RingExperiment.api_size Flunct.Sparse 512;

"Ring code size Church";
RingExperiment.api_size Flunct.Church 2;
RingExperiment.api_size Flunct.Church 4;
RingExperiment.api_size Flunct.Church 8;
RingExperiment.api_size Flunct.Church 16;
RingExperiment.api_size Flunct.Church 32;
RingExperiment.api_size Flunct.Church 64;
RingExperiment.api_size Flunct.Church 128;
RingExperiment.api_size Flunct.Church 256;
RingExperiment.api_size Flunct.Church 512;

"Ring code size Tabulation";
RingExperiment.api_size Flunct.Tabulation 2;
RingExperiment.api_size Flunct.Tabulation 4;
RingExperiment.api_size Flunct.Tabulation 8;
RingExperiment.api_size Flunct.Tabulation 16;
RingExperiment.api_size Flunct.Tabulation 32;
RingExperiment.api_size Flunct.Tabulation 64;
RingExperiment.api_size Flunct.Tabulation 128;
RingExperiment.api_size Flunct.Tabulation 256;
RingExperiment.api_size Flunct.Tabulation 512;
*)

(*
 * Ring compilation time
 *)

(*
"Ring compilation time 16 Sparse";
RingExperiment.run_time Flunct.Sparse 16 0;
RingExperiment.run_time Flunct.Sparse 16 16;
RingExperiment.run_time Flunct.Sparse 16 32;
RingExperiment.run_time Flunct.Sparse 16 64;
RingExperiment.run_time Flunct.Sparse 16 128;
RingExperiment.run_time Flunct.Sparse 16 256;
RingExperiment.run_time Flunct.Sparse 16 512;
RingExperiment.run_time Flunct.Sparse 16 1024;
RingExperiment.run_time Flunct.Sparse 16 2048;
RingExperiment.run_time Flunct.Sparse 16 4096;
RingExperiment.run_time Flunct.Sparse 16 8192;
RingExperiment.run_time Flunct.Sparse 16 16384;
RingExperiment.run_time Flunct.Sparse 16 32768;

"Ring compilation time 16 Church";
RingExperiment.run_time Flunct.Church 16 0;
RingExperiment.run_time Flunct.Church 16 16;
RingExperiment.run_time Flunct.Church 16 32;
RingExperiment.run_time Flunct.Church 16 64;
RingExperiment.run_time Flunct.Church 16 128;
RingExperiment.run_time Flunct.Church 16 256;
RingExperiment.run_time Flunct.Church 16 512;

"Ring compilation time 16 Tabulation";
RingExperiment.run_time Flunct.Tabulation 16 0;
RingExperiment.run_time Flunct.Tabulation 16 16;
RingExperiment.run_time Flunct.Tabulation 16 32;
RingExperiment.run_time Flunct.Tabulation 16 64;
RingExperiment.run_time Flunct.Tabulation 16 128;
RingExperiment.run_time Flunct.Tabulation 16 256;
RingExperiment.run_time Flunct.Tabulation 16 512;
RingExperiment.run_time Flunct.Tabulation 16 1024;
RingExperiment.run_time Flunct.Tabulation 16 2048;
RingExperiment.run_time Flunct.Tabulation 16 4096;
RingExperiment.run_time Flunct.Tabulation 16 8192;

"Ring compilation time Sparse";
RingExperiment.run_time Flunct.Sparse 2 0;
RingExperiment.run_time Flunct.Sparse 4 0;
RingExperiment.run_time Flunct.Sparse 8 0;
RingExperiment.run_time Flunct.Sparse 16 0;
RingExperiment.run_time Flunct.Sparse 32 0;
RingExperiment.run_time Flunct.Sparse 64 0;
RingExperiment.run_time Flunct.Sparse 128 0;
RingExperiment.run_time Flunct.Sparse 256 0;
RingExperiment.run_time Flunct.Sparse 512 0;

"Ring compilation time Church";
RingExperiment.run_time Flunct.Church 2 0;
RingExperiment.run_time Flunct.Church 4 0;
RingExperiment.run_time Flunct.Church 8 0;
RingExperiment.run_time Flunct.Church 16 0;
RingExperiment.run_time Flunct.Church 32 0;
RingExperiment.run_time Flunct.Church 64 0;
RingExperiment.run_time Flunct.Church 128 0;

"Ring compilation time Tabulation";
RingExperiment.run_time Flunct.Tabulation 2 0;
RingExperiment.run_time Flunct.Tabulation 4 0;
RingExperiment.run_time Flunct.Tabulation 8 0;
RingExperiment.run_time Flunct.Tabulation 16 0;
RingExperiment.run_time Flunct.Tabulation 32 0;
RingExperiment.run_time Flunct.Tabulation 64 0;
RingExperiment.run_time Flunct.Tabulation 128 0;
RingExperiment.run_time Flunct.Tabulation 256 0;
RingExperiment.run_time Flunct.Tabulation 512 0;
*)

(*
 * HTML compilation time
 *)

(*
"HTML compilation time";
HtmlExperiment.run_time 0;
HtmlExperiment.run_time 1;
HtmlExperiment.run_time 2;
HtmlExperiment.run_time 4;
HtmlExperiment.run_time 8;
HtmlExperiment.run_time 16;
HtmlExperiment.run_time 32;
HtmlExperiment.run_time 64;
HtmlExperiment.run_time 128;
HtmlExperiment.run_time 256;
*)

(*
 * DCFL compilation time
 *)

(*
"DCFL MLton";
DcflExperiment.run_time DcflExperiment.MLton 2;
DcflExperiment.run_time DcflExperiment.MLton 4;
DcflExperiment.run_time DcflExperiment.MLton 8;
DcflExperiment.run_time DcflExperiment.MLton 16;
DcflExperiment.run_time DcflExperiment.MLton 32;
DcflExperiment.run_time DcflExperiment.MLton 64;
DcflExperiment.run_time DcflExperiment.MLton 128;
DcflExperiment.run_time DcflExperiment.MLton 256;
DcflExperiment.run_time DcflExperiment.MLton 512;
DcflExperiment.run_time DcflExperiment.MLton 1024;

"DCFL ELM";
DcflExperiment.run_time DcflExperiment.ELM 2;
DcflExperiment.run_time DcflExperiment.ELM 4;
DcflExperiment.run_time DcflExperiment.ELM 8;
DcflExperiment.run_time DcflExperiment.ELM 16;
DcflExperiment.run_time DcflExperiment.ELM 32;
DcflExperiment.run_time DcflExperiment.ELM 64;
DcflExperiment.run_time DcflExperiment.ELM 128;
DcflExperiment.run_time DcflExperiment.ELM 256;
DcflExperiment.run_time DcflExperiment.ELM 512;
DcflExperiment.run_time DcflExperiment.ELM 1024;
DcflExperiment.run_time DcflExperiment.ELM 2048;
DcflExperiment.run_time DcflExperiment.ELM 4096;
DcflExperiment.run_time DcflExperiment.ELM 8192;
DcflExperiment.run_time DcflExperiment.ELM 16384;
*)
