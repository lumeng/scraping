(* Wolfram Language Package *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: Scraping *)
(* :Context: Scraping` *)
(* :Author: meng *)
(* :Date: 2021-01-16 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.1 *)
(* :Copyright: (c) 2021 meng *)
(* :Keywords: *)
(* :Discussion: *)

(* For new style packages see: https://mathematica.stackexchange.com/a/176489) *)
(* Declare package context *)
Package["Scraping`"]

(* Import other packages *)
PackageImport["GeneralUtilities`"]

(* Keep function package private *)
PackageScope["privateFunc"]
privateFunc[x_] := x^2;

(* Export functions *)
PackageExport["exportedFunc"]
exportedFunc[] := "Hello World";
