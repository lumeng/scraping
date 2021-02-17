(* Wolfram Language Package *)

BeginPackage["Scraping`"];

Needs["meng`Shared`"];
Needs["meng`Image`"];

Scrape::usage = "Scrape[url] scrapes specific elements from a resource URL.";

Begin[ "Private`"];


ClearAll[Scrape];
ClearAll[regexData];



regexData["SourceURL", "artron.net"] = RegularExpression["https?://zxp\\.artron\\.net/specials/goods/goodsdetail/[0-9]+"];

regexData["ImageURL", "artron.net"] = RegularExpression["https?://img\\d+\\.artimg\\.net/zxp/auctions/\\d+/\\d+/.*\\.jpg"];

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
        {all = #[[2]][[3;;]], d = #[[1]]},
        (
            Pause[RandomReal[2]];
            mengDownloadImage[
                #,
                "FileBaseName" -> StringJoin[
                    {
                        d[["author"]],
                        "__",
                        d[["title"]],
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
                        getDomainName[d[["url"]]],
                        "_",
                        d[["publisher"]],
                        "_",
                        FileNameTake[#, -1]
                    }
                ],
                "Subdirectory" -> {getDomainName[d[["url"]]]},
                "FileNameRenameRules" -> {
                    RegularExpression["img\\d+\\.artimg\\.net"] -> "artron.net"
                }
            ]
        ) & /@ all
    ] &,
    {#, Scrape[#, "ImageLinks"]} &
][dataEntry];

getDomainName[url_String] := RightComposition[
    FileNameSplit,
    #[[3]] &
][url];

End[];

EndPackage[];

