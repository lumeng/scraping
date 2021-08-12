(* Wolfram Language Package *)

BeginPackage["Scraping`"];

Needs["meng`Shared`"];
Needs["meng`Image`"];

Scrape::usage = "Scrape[url] scrapes specific elements from a resource URL.";

Begin[ "Private`"];


ClearAll[Scrape];
ClearAll[regexData];
ClearAll[formatFileNameElement];

(******************************************************************************)
(* macaulaylibrary.org *)

regexData["SourceURL", "macaulaylibrary.org"] = RegularExpression["https?://macaulaylibrary\\.org/asset/[0-9]+"];

regexData["ImageURL", "macaulaylibrary.org"] = RegularExpression["https?://img\\d+\\.artimg\\.net/zxp/auctions/.*/.*\\.(jpg|jpeg|png)"];

Scrape[
    dataEntry_Dataset?(StringQ[#[["url"]]]
    && StringMatchQ[#[["url"]], regexData["SourceURL", "macaulaylibrary.org"]] &),
    "ImageLinks"
] := Composition[
    DeleteDuplicates,
    Select[
        #,
        StringQ[#] && StringMatchQ[#, RegularExpression["https://cdn\\..*"]] &
    ] &,
    Import[#, "Hyperlinks"] &,
    #[["url"]] &
][dataEntry];

Scrape[
    dataEntry_Dataset?(StringQ[#[["url"]]]
    && StringMatchQ[#[["url"]], regexData["SourceURL", "macaulaylibrary.org"]] & )
] := Composition[
    With[
        {urls = #[[2]][[;;]], d = #[[1]]},
        (
            Pause[RandomReal[2]];
            mengDownloadImage[
                #,
                "FileBaseName" -> StringJoin[
                    {
                        formatFileNameElement[d[["author"]]],
                        "__",
                        formatFileNameElement[d[["title"]]],
                        "_",
                        IntegerString[First[Position[urls, #]]],
                        "__",
                        Which[
                            DateObjectQ[Normal[d[["authoring_date"]]]],
                            DateString[Normal[d[["authoring_date"]]], "ISODate"],
                            StringQ[Normal[d[["authoring_date"]]]],
                            Normal[d[["authoring_date"]]],
                            DateObjectQ[Normal[d[["publishing_date"]]]],
                            DateString[Normal[d[["publishing_date"]]], "ISODate"],
                            StringQ[Normal[d[["publishing_date"]]]],
                            Normal[d[["publishing_date"]]],
                            DateObjectQ[Normal[d[["retrieving_date"]]]],
                            DateString[Normal[d[["retrieving_date"]]], "ISODate"],
                            StringQ[Normal[d[["retrieving_date"]]]],
                            Normal[d[["retrieving_date"]]],
                            True,
                            DateString["ISODate"] <> "_(retrieved)"
                        ],
                        "__",
                        formatFileNameElement[getDomainName[d[["url"]]]],
                        "_",
                        formatFileNameElement[d[["publisher"]]],
                        "_",
                        FileNameTake[#, -1],
                        If[
                            !StringMatchQ[
                                FileNameTake[#, -1],
                                RegularExpression[".*\\.\\w{3,}"]
                            ],
                            "." <> ToLowerCase@FileFormat[
                                URLDownload[
                                    #,
                                    FileNameJoin[{$TemporaryDirectory, "tmp" <> FileNameTake[#, -1]}]
                                ]
                            ],
                            ""
                        ]
                    }
                ],
                "Subdirectory" -> {getDomainName[d[["url"]]]},
                "FileNameRenameRules" -> {
                    RegularExpression["img\\d+\\.artimg\\.net"] -> "artron.net"
                }
            ]
        ) & /@ urls
    ] &,
    {#, Scrape[#, "ImageLinks"]} &
][dataEntry];

(******************************************************************************)
(* artron.net *)

regexData["SourceURL", "artron.net"] = RegularExpression["https?://zxp\\.artron\\.net/specials/goods/goodsdetail/[0-9]+"];

regexData["ImageURL", "artron.net"] = RegularExpression["https?://img\\d+\\.artimg\\.net/zxp/auctions/.*/.*\\.(jpg|jpeg|png)"];

Scrape[
    dataEntry_Dataset?(StringQ[#[["url"]]]
    && StringMatchQ[#[["url"]], regexData["SourceURL", "artron.net"]] &),
    "ImageLinks"
] := Composition[
    {Sort, Identity}[[2]],
    DeleteDuplicates,
    Flatten,
    StringCases[#, regexData["ImageURL", "artron.net"]] & /@ # &,
    Import[#, "ImageLinks"] &,
    #[["url"]] &
][dataEntry];

Scrape[
    dataEntry_Dataset?(StringQ[#[["url"]]]
    && (StringMatchQ[#[["url"]], regexData["SourceURL", "artron.net"]] ||
        StringMatchQ[#[["url"]], regexData["SourceURL", "gallery.artron.net"]] )& )
] := Composition[
    With[
        {urls = #[[2]][[;;]], d = #[[1]]},
        (
            Pause[RandomReal[2]];
            mengDownloadImage[
                #,
                "FileBaseName" -> StringJoin[
                    {
                        formatFileNameElement[d[["author"]]],
                        "__",
                        formatFileNameElement[d[["title"]]],
                        "_",
                        IntegerString[First[Position[urls, #]]],
                        "__",
                        Which[
                            DateObjectQ[Normal[d[["authoring_date"]]]],
                            DateString[Normal[d[["authoring_date"]]], "ISODate"],
                            StringQ[Normal[d[["authoring_date"]]]],
                            Normal[d[["authoring_date"]]],
                            DateObjectQ[Normal[d[["publishing_date"]]]],
                            DateString[Normal[d[["publishing_date"]]], "ISODate"],
                            StringQ[Normal[d[["publishing_date"]]]],
                            Normal[d[["publishing_date"]]],
                            DateObjectQ[Normal[d[["retrieving_date"]]]],
                            DateString[Normal[d[["retrieving_date"]]], "ISODate"],
                            StringQ[Normal[d[["retrieving_date"]]]],
                            Normal[d[["retrieving_date"]]],
                            True,
                            DateString["ISODate"] <> "(retrieved)"
                        ],                        "__",
                        formatFileNameElement[getDomainName[d[["url"]]]],
                        "_",
                        formatFileNameElement[d[["publisher"]]],
                        "_",
                        FileNameTake[#, -1]
                    }
                ],
                "Subdirectory" -> {"topic", getDomainName[d[["url"]]]},
                "FileNameRenameRules" -> {
                    RegularExpression["img\\d+\\.artimg\\.net"] -> "artron.net"
                }
            ]
        ) & /@ urls
    ] &,
    {#, Scrape[#, "ImageLinks"]} &
][dataEntry];


(******************************************************************************)
(* gallery.artron.net *)

regexData["SourceURL", "gallery.artron.net"] = RegularExpression["https?://gallery\\.artron\\.net/works/.*"];

regexData["ImageURL", "gallery.artron.net"] = RegularExpression["https?://img\\d+.artimg.net/gallery/.*/.*\\d+\\.(jpg|jpeg|png)"];

Scrape[
    dataEntry_Dataset?(StringQ[#[["url"]]]
        && StringMatchQ[#[["url"]], regexData["SourceURL", "gallery.artron.net"]] &),
    "ImageLinks"
] := Composition[
    {Sort, Identity}[[2]],
    DeleteDuplicates,
    Flatten,
    Cases[#, XMLElement["img", {"id" -> "bigPic", "src" -> imgURL_String, ___}, {}] :> imgURL, Infinity] &,
    Import[#, "XMLObject"] &,
    #[["url"]] &
][dataEntry];



(******************************************************************************)
(* http://g2.ltfc.net *)
(* example: http://g2.ltfc.net/pageview?id=608986c399d736503894476b&src=SUHA
*)

regexData["SourceURL", "g2.ltfc.net"] = RegularExpression["https?://g2\\.ltfc\\.net/.*"];

(* example: https://cag.ltfc.net/cagstore/5aeca57e54abff7ca62f5d43/16/0_0.jpg?&sign=22fe45a91a279c9af61f25e489233588&t=61be7600 *)
regexData["ImageURL", "g2.ltfc.net"] = RegularExpression["https?://cag\\.ltfc\\.net/cagstore/[0-9a-z]+/\\d+/\\d_\\d\\.(jpg|jpeg|png)\\?&sign=[0-9a-z]+&t=[0-9a-z]+"];

With[
    {url = "https://cag.ltfc.net/cagstore/5aeca57e54abff7ca62f5d43/16/0_0.jpg?&sign=22fe45a91a279c9af61f25e489233588&t=61be7600"},
    Assert[StringMatchQ[url, regexData["ImageURL", "g2.ltfc.net"]]]
];

Scrape[
    dataEntry_Dataset?(StringQ[#[["url"]]]
        && StringMatchQ[#[["url"]], regexData["SourceURL", "g2.ltfc.net"]] &),
    "ImageLinks"
] := Composition[
    {Sort, Identity}[[2]],
    DeleteDuplicates,
    Flatten,
    Cases[#, XMLElement["img", {"id" -> "bigPic", "src" -> imgURL_String, ___}, {}] :> imgURL, Infinity] &,
    Import[#, "XMLObject"] &,
    #[["url"]] &
][dataEntry];

(******************************************************************************)
(* helpers *)

getDomainName[url_String] := RightComposition[
    FileNameSplit,
    #[[3]] &
][url];

formatFileNameElement[x_Dataset] := formatFileNameElement[Normal[x]];

formatFileNameElement[x_] := Switch[
    x,
    _Missing, "(missing)",
    _String, x,
    _, ToString[x]
];

End[];

EndPackage[];

