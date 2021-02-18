(* Wolfram Language Package *)

BeginPackage["Scraping`"];

Needs["meng`Shared`"];
Needs["meng`Image`"];

Scrape::usage = "Scrape[url] scrapes specific elements from a resource URL.";

Begin[ "Private`"];


ClearAll[Scrape];
ClearAll[regexData];



regexData["SourceURL", "artron.net"] = RegularExpression["https?://zxp\\.artron\\.net/specials/goods/goodsdetail/[0-9]+"];

regexData["ImageURL", "artron.net"] = RegularExpression["https?://img\\d+\\.artimg\\.net/zxp/auctions/.*/.*\\.jpg"];

Scrape[
    dataEntry_Dataset?(StringQ[#[["url"]]] && StringMatchQ[#[["url"]], regexData["SourceURL", "artron.net"]] &),
    "ImageLinks"
] := Composition[
    {Sort, Identity}[[1]],
(******************************************************************************)
(* artron.net *)

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
    && StringMatchQ[#[["url"]], regexData["SourceURL", "artron.net"]] & )
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
                            DateObjectQ[d[["authoring_date"]]],
                            DateString[d[["authoring_date"]]],
                            StringQ[d[["authoring_date"]]],
                            d[["authoring_date"]],
                            DateObjectQ[d[["publishing_date"]]],
                            DateString[d[["publishing_date"]]],
                            StringQ[d[["publishing_date"]]],
                            d[["publishing_date"]],
                            DateObjectQ[d[["retrieving_date"]]],
                            DateString[d[["retrieving_date"]]],
                            StringQ[d[["retrieving_date"]]],
                            d[["retrieving_date"]],
                            True,
                            DateString["ISODate"]
                        ],
                        "__",
                        formatFileNameElement[getDomainName[d[["url"]]]],
                        "_",
                        formatFileNameElement[d[["publisher"]]],
                        "_",
                        FileNameTake[#, -1]
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


Scrape[
    dataEntry_Dataset?(StringQ[#[["url"]]]
    && StringMatchQ[#[["url"]], regexData["SourceURL", "artron.net"]] & )
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
                            DateObjectQ[d[["authoring_date"]]],
                            DateString[d[["authoring_date"]]],
                            StringQ[d[["authoring_date"]]],
                            d[["authoring_date"]],
                            DateObjectQ[d[["publishing_date"]]],
                            DateString[d[["publishing_date"]]],
                            StringQ[d[["publishing_date"]]],
                            d[["publishing_date"]],
                            DateObjectQ[d[["retrieving_date"]]],
                            DateString[d[["retrieving_date"]]],
                            StringQ[d[["retrieving_date"]]],
                            d[["retrieving_date"]],
                            True,
                            DateString["ISODate"]
                        ],
                        "__",
                        formatFileNameElement[getDomainName[d[["url"]]]],
                        "_",
                        formatFileNameElement[d[["publisher"]]],
                        "_",
                        FileNameTake[#, -1]
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

getDomainName[url_String] := RightComposition[
    FileNameSplit,
    #[[3]] &
][url];

formatFileNameElement[x_] := Switch[
    x,
    _Missing, "(missing)",
    _String, x,
    _, ToString[x]
];

End[];

EndPackage[];

