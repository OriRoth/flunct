CM.make "experiments.cm";

(*
 * Ring: code size
 *)

"Fig. B.2. Ring FSM size vs. fluent API size";

"Ring code size Shuffle";
(2, RingExperiment.api_size Flunct.Shuffle 2);
(4, RingExperiment.api_size Flunct.Shuffle 4);
(8, RingExperiment.api_size Flunct.Shuffle 8);
(16, RingExperiment.api_size Flunct.Shuffle 16);

"Ring code size Sparse";
(2, RingExperiment.api_size Flunct.Sparse 2);
(4, RingExperiment.api_size Flunct.Sparse 4);
(8, RingExperiment.api_size Flunct.Sparse 8);
(16, RingExperiment.api_size Flunct.Sparse 16);
(32, RingExperiment.api_size Flunct.Sparse 32);
(64, RingExperiment.api_size Flunct.Sparse 64);
(128, RingExperiment.api_size Flunct.Sparse 128);
(256, RingExperiment.api_size Flunct.Sparse 256);
(512, RingExperiment.api_size Flunct.Sparse 512);

"Ring code size Church";
(2, RingExperiment.api_size Flunct.Church 2);
(4, RingExperiment.api_size Flunct.Church 4);
(8, RingExperiment.api_size Flunct.Church 8);
(16, RingExperiment.api_size Flunct.Church 16);
(32, RingExperiment.api_size Flunct.Church 32);
(64, RingExperiment.api_size Flunct.Church 64);
(128, RingExperiment.api_size Flunct.Church 128);
(256, RingExperiment.api_size Flunct.Church 256);
(512, RingExperiment.api_size Flunct.Church 512);

"Ring code size Tabulation";
(2, RingExperiment.api_size Flunct.Tabulation 2);
(4, RingExperiment.api_size Flunct.Tabulation 4);
(8, RingExperiment.api_size Flunct.Tabulation 8);
(16, RingExperiment.api_size Flunct.Tabulation 16);
(32, RingExperiment.api_size Flunct.Tabulation 32);
(64, RingExperiment.api_size Flunct.Tabulation 64);
(128, RingExperiment.api_size Flunct.Tabulation 128);
(256, RingExperiment.api_size Flunct.Tabulation 256);
(512, RingExperiment.api_size Flunct.Tabulation 512);

(*
 * Ring compilation time
 *)

"Fig. B.3. Chain length vs. compilation time with 16-state Ring FSM and MLton";

"Ring compilation time 16 Sparse";
(0, RingExperiment.run_time Flunct.Sparse 16 0);
(16, RingExperiment.run_time Flunct.Sparse 16 16);
(32, RingExperiment.run_time Flunct.Sparse 16 32);
(64, RingExperiment.run_time Flunct.Sparse 16 64);
(128, RingExperiment.run_time Flunct.Sparse 16 128);
(256, RingExperiment.run_time Flunct.Sparse 16 256);
(512, RingExperiment.run_time Flunct.Sparse 16 512);
(1024, RingExperiment.run_time Flunct.Sparse 16 1024);
(2048, RingExperiment.run_time Flunct.Sparse 16 2048);
(4096, RingExperiment.run_time Flunct.Sparse 16 4096);
(8192, RingExperiment.run_time Flunct.Sparse 16 8192);
(16384, RingExperiment.run_time Flunct.Sparse 16 16384);
(32768, RingExperiment.run_time Flunct.Sparse 16 32768);

"Ring compilation time 16 Church";
(0, RingExperiment.run_time Flunct.Church 16 0);
(16, RingExperiment.run_time Flunct.Church 16 16);
(32, RingExperiment.run_time Flunct.Church 16 32);
(64, RingExperiment.run_time Flunct.Church 16 64);
(128, RingExperiment.run_time Flunct.Church 16 128);
(256, RingExperiment.run_time Flunct.Church 16 256);
(512, RingExperiment.run_time Flunct.Church 16 512);

"Ring compilation time 16 Tabulation";
(0, RingExperiment.run_time Flunct.Tabulation 16 0);
(16, RingExperiment.run_time Flunct.Tabulation 16 16);
(32, RingExperiment.run_time Flunct.Tabulation 16 32);
(64, RingExperiment.run_time Flunct.Tabulation 16 64);
(128, RingExperiment.run_time Flunct.Tabulation 16 128);
(256, RingExperiment.run_time Flunct.Tabulation 16 256);
(512, RingExperiment.run_time Flunct.Tabulation 16 512);
(1024, RingExperiment.run_time Flunct.Tabulation 16 1024);
(2048, RingExperiment.run_time Flunct.Tabulation 16 2048);
(4096, RingExperiment.run_time Flunct.Tabulation 16 4096);
(8192, RingExperiment.run_time Flunct.Tabulation 16 8192);

"Fig. B.4. Ring FSM size vs. compilation time of the empty chain (^^ $$) with MLton";

"Ring API compilation time Sparse";
(2, RingExperiment.run_time Flunct.Sparse 2 0);
(4, RingExperiment.run_time Flunct.Sparse 4 0);
(8, RingExperiment.run_time Flunct.Sparse 8 0);
(16, RingExperiment.run_time Flunct.Sparse 16 0);
(32, RingExperiment.run_time Flunct.Sparse 32 0);
(64, RingExperiment.run_time Flunct.Sparse 64 0);
(128, RingExperiment.run_time Flunct.Sparse 128 0);
(256, RingExperiment.run_time Flunct.Sparse 256 0);
(512, RingExperiment.run_time Flunct.Sparse 512 0);

"Ring API compilation time Church";
(2, RingExperiment.run_time Flunct.Church 2 0);
(4, RingExperiment.run_time Flunct.Church 4 0);
(8, RingExperiment.run_time Flunct.Church 8 0);
(16, RingExperiment.run_time Flunct.Church 16 0);
(32, RingExperiment.run_time Flunct.Church 32 0);
(64, RingExperiment.run_time Flunct.Church 64 0);
(128, RingExperiment.run_time Flunct.Church 128 0);

"Ring API compilation time Tabulation";
(2, RingExperiment.run_time Flunct.Tabulation 2 0);
(4, RingExperiment.run_time Flunct.Tabulation 4 0);
(8, RingExperiment.run_time Flunct.Tabulation 8 0);
(16, RingExperiment.run_time Flunct.Tabulation 16 0);
(32, RingExperiment.run_time Flunct.Tabulation 32 0);
(64, RingExperiment.run_time Flunct.Tabulation 64 0);
(128, RingExperiment.run_time Flunct.Tabulation 128 0);
(256, RingExperiment.run_time Flunct.Tabulation 256 0);
(512, RingExperiment.run_time Flunct.Tabulation 512 0);

(*
 * HTML compilation time
 *)

"Fig. B.6. Example HTML size vs. compilation time with MLton";

"HTML compilation time";
(0, HtmlExperiment.run_time 0);
(1, HtmlExperiment.run_time 1);
(2, HtmlExperiment.run_time 2);
(4, HtmlExperiment.run_time 4);
(8, HtmlExperiment.run_time 8);
(16, HtmlExperiment.run_time 16);
(32, HtmlExperiment.run_time 32);
(64, HtmlExperiment.run_time 64);
(128, HtmlExperiment.run_time 128);
(256, HtmlExperiment.run_time 256);

(*
 * DCFL compilation time
 *)

"B.7. Chain length vs. compilation time when compiling the fluent API in List. 6.4 in SML";

"DCFL MLton";
(2, DcflExperiment.run_time DcflExperiment.MLton 2);
(4, DcflExperiment.run_time DcflExperiment.MLton 4);
(8, DcflExperiment.run_time DcflExperiment.MLton 8);
(16, DcflExperiment.run_time DcflExperiment.MLton 16);
(32, DcflExperiment.run_time DcflExperiment.MLton 32);
(64, DcflExperiment.run_time DcflExperiment.MLton 64);
(128, DcflExperiment.run_time DcflExperiment.MLton 128);
(256, DcflExperiment.run_time DcflExperiment.MLton 256);
(512, DcflExperiment.run_time DcflExperiment.MLton 512);
(1024, DcflExperiment.run_time DcflExperiment.MLton 1024);

"DCFL ELM";
(2, DcflExperiment.run_time DcflExperiment.ELM 2);
(4, DcflExperiment.run_time DcflExperiment.ELM 4);
(8, DcflExperiment.run_time DcflExperiment.ELM 8);
(16, DcflExperiment.run_time DcflExperiment.ELM 16);
(32, DcflExperiment.run_time DcflExperiment.ELM 32);
(64, DcflExperiment.run_time DcflExperiment.ELM 64);
(128, DcflExperiment.run_time DcflExperiment.ELM 128);
(256, DcflExperiment.run_time DcflExperiment.ELM 256);
(512, DcflExperiment.run_time DcflExperiment.ELM 512);
(1024, DcflExperiment.run_time DcflExperiment.ELM 1024);
(2048, DcflExperiment.run_time DcflExperiment.ELM 2048);
(4096, DcflExperiment.run_time DcflExperiment.ELM 4096);
(8192, DcflExperiment.run_time DcflExperiment.ELM 8192);
(16384, DcflExperiment.run_time DcflExperiment.ELM 16384);

val _ = OS.Process.exit OS.Process.success
