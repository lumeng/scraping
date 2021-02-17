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
    Flatten,
    StringCases[#, regexData["ImageURL", "artron.net"]] & /@ # &,
    Import[#, "ImageLinks"] &,
    #[["url"]] &
][dataEntry];

Scrape[
    dataEntry_Dataset?(StringQ[#[["url"]]] && StringMatchQ[#[["url"]], regexData["SourceURL", "artron.net"]] & )
] := Composition[
    With[
        {urls = #[[2]][[3;;]], d = #[[1]]},
        (
            Pause[RandomReal[2]];
            mengDownloadImage[
                #,
                "FileBaseName" -> StringJoin[
                    {
                        formatFileNameElement[d[["author"]]],
                        "__",
                        formatFileNameElement[d[["title"]]],
                        "__",
                        Which[
                            StringQ[d[["authoring_date"]]],
                            d[["authoring_date"]],
                            StringQ[d[["publishing_date"]]],
                            d[["publishing_date"]],
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

