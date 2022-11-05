# Flunct&mdash;A Functional Fluent API Generator

Flunct embeds domain-specific languages (DSLs) in SML.
Flunct compiles a given FSM <i>M</i> specifying the DSL syntax into
a functional fluent API that validates <i>M</i> at compile time,
so only legal DSL programs may compile.
For example, we can used Flunct to embed a subset of HTML in SML,
making it possible to write HTML webpages as part of an SML code:

```sml
open HTML

val webpage = ^^
    <html>
    	<body>
    		<h1> `"National Parks" </h1>
    		`"California:"
    		<table>
    			<tr>
    				<th> `"Park Description" </th>
    				<th> `"Park Picture" </th>
    			</tr>
    			<tr>
    				<td> <p> <b> `"Yosemite" </b> `"national park" </p> </td>
    				<td> <img src "https://tinyurl.com/yosemite5"/> </td>
    			</tr>
    		</table>
    	</body>
    </html>
$$
```

You can find this and other examples in `flunct/examples`.

## Code

The Flunct source is found in `flunct/src`.
The code is fully documented.

## Tests

The tests are found in `flunct/tests`.
To run the tests, run `run_all.sml`.

## Experiments

We used Flunct to conduct a series of experiments to measure the
code size and compilation time of the three fluent API generation
methods.
The experiment sources are found in `flunct/experiments`.
To run an experiment, uncomment the relevant code in
`run_experiments.sml` and run the file.







