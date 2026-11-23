! SPDX-License-Identifier: GPL-3.0-only
! References page data. Generated from the project reference list (UTF-8 text as achar byte sequences).
module native_references
 implicit none
 integer, parameter :: nref=122, ref_len=946
 integer, parameter :: ref_section(nref)=[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3]
 character(len=48), parameter :: section_title(3)=[character(len=48):: "Literature, standards and repositories", &
  "Software and specifications used in this edition","Edition notes and controls"]
 character(len=ref_len), parameter :: ref_text(nref)=[character(len=ref_len):: &
  "Ames, T., Koons, L., Sall, K. and Warsaw, C. (2000). Using XML and Java for Astronomical" // &
   " Instrumentation Control. Available at: https://ntrs.nasa.gov/citations/20000082015 (Acc" // &
   "essed: 21 November 2026).", &
  "andreww (n.d.). andreww/fox [GitHub repository]. Available at: https://github.com/andrew" // &
   "w/fox (Accessed: 21 November 2026).", &
  "Barbau-Piednoir, E., Stragier, P., Roosens, N., Mazzara, M., Savini, C., Van den Eede, G" // &
   ". and Van den Bulcke, M. (2014). 'Inter-laboratory Testing of GMO Detection by Combinato" // &
   "ry SYBR" // &
   achar(194)//achar(174) // &
   "Green PCR Screening (CoSYPS)', Food Analytical Methods. Available at: https://doi.org/10" // &
   ".1007/s12161-014-9837-3 (Accessed: 21 November 2026).", &
  "Blecher, S., Horton, M. and Norrod, V. (1970). A self-study course in FORTRAN programmin" // &
   "g. Volume 1 - Textbook. Available at: https://ntrs.nasa.gov/citations/19700015982 (Acces" // &
   "sed: 21 November 2026).", &
  "Bustin, S.A., Benes, V., Garson, J.A., Hellemans, J., Huggett, J., Kubista, M., Mueller," // &
   " R., Nolan, T., Pfaffl, M.W., Shipley, G.L., Vandesompele, J. and Wittwer, C.T. (2009). " // &
   "'The MIQE Guidelines: Minimum Information for Publication of Quantitative Real-Time PCR " // &
   "Experiments', Clinical Chemistry, 55(4), pp. 611-622. Available at: https://doi.org/10.1" // &
   "373/clinchem.2008.112797 (Accessed: 21 November 2026).", &
  "Bustin, S.A., Ruijter, J.M., van den Hoff, M.J.B., Kubista, M., Pfaffl, M.W., Shipley, G" // &
   ".L., Tran, N., R" // &
   achar(195)//achar(182) // &
   "diger, S., Untergasser, A., Mueller, R., Nolan, T., Milavec, M., Burns, M.J., Huggett, J" // &
   ".F., Vandesompele, J. and Wittwer, C.T. (2025). 'MIQE 2.0: Revision of the Minimum Infor" // &
   "mation for Publication of Quantitative Real-Time PCR Experiments Guidelines', Clinical C" // &
   "hemistry, 71(6), pp. 634-651. Available at: https://doi.org/10.1093/clinchem/hvaf043 (Ac" // &
   "cessed: 21 November 2026).", &
  "Carroll, S.R., Garba, I., Figueroa-Rodr" // &
   achar(195)//achar(173) // &
   "guez, O.L., Holbrook, J., Lovett, R., Materechera, S., Parsons, M., Raseroka, K., Rodrig" // &
   "uez-Lonebear, D., Rowe, R., Sara, R., Walker, J.D., Anderson, J. and Hudson, M. (2020). " // &
   "'The CARE Principles for Indigenous Data Governance', Data Science Journal, 19, article " // &
   "43. Available at: https://doi.org/10.5334/dsj-2020-043 (Accessed: 21 November 2026).", &
  "chaibio (n.d.). chaibio/chaipcr [GitHub repository]. Available at: https://github.com/ch" // &
   "aibio/chaipcr (Accessed: 21 November 2026).", &
  "Consultative Committee for Space Data Systems (2002). DEDSL - XML/DTD Syntax (CCSDS 647." // &
   "3-B-1). Available at: https://public.ccsds.org/Pubs/647x3b1.pdf (Accessed: 21 November 2" // &
   "026).", &
  "Consultative Committee for Space Data Systems (2008). XML Formatted Data Unit (XFDU) Str" // &
   "ucture and Construction Rules (CCSDS 661.0-B-1). Available at: https://public.ccsds.org/" // &
   "Pubs/661x0b1.pdf (Accessed: 21 November 2026).", &
  "Consultative Committee for Space Data Systems (2009a). Spacecraft Onboard Interface Serv" // &
   "ices" // &
   achar(226)//achar(128)//achar(148) // &
   "Subnetwork Device Discovery Service (CCSDS 854.0-M-1). Available at: https://public.ccsd" // &
   "s.org/Pubs/854x0m1.pdf (Accessed: 21 November 2026).", &
  "Consultative Committee for Space Data Systems (2009b). Spacecraft Onboard Interface Serv" // &
   "ices" // &
   achar(226)//achar(128)//achar(148) // &
   "Subnetwork Memory Access Service (CCSDS 852.0-M-1). Available at: https://public.ccsds.o" // &
   "rg/Pubs/852x0m1.pdf (Accessed: 21 November 2026).", &
  "Consultative Committee for Space Data Systems (2009c). Spacecraft Onboard Interface Serv" // &
   "ices" // &
   achar(226)//achar(128)//achar(148) // &
   "Subnetwork Packet Service (CCSDS 851.0-M-1). Available at: https://public.ccsds.org/Pubs" // &
   "/851x0m1.pdf (Accessed: 21 November 2026).", &
  "Consultative Committee for Space Data Systems (2009d). Spacecraft Onboard Interface Serv" // &
   "ices" // &
   achar(226)//achar(128)//achar(148) // &
   "Subnetwork Synchronisation Service (CCSDS 853.0-M-1). Available at: https://public.ccsds" // &
   ".org/Pubs/853x0m1.pdf (Accessed: 21 November 2026).", &
  "Consultative Committee for Space Data Systems (2009e). Spacecraft Onboard Interface Serv" // &
   "ices" // &
   achar(226)//achar(128)//achar(148) // &
   "Subnetwork Test Service (CCSDS 855.0-M-1). Available at: https://public.ccsds.org/Pubs/8" // &
   "55x0m1.pdf (Accessed: 21 November 2026).", &
  "Consultative Committee for Space Data Systems (2012). Spacecraft Onboard Interface Servi" // &
   "ces" // &
   achar(226)//achar(128)//achar(148) // &
   "RFID-Based Inventory Management Systems (CCSDS 881.0-M-1). Available at: https://public." // &
   "ccsds.org/Pubs/881x0m1.pdf (Accessed: 21 November 2026).", &
  "Consultative Committee for Space Data Systems (2013a). Spacecraft Onboard Interface Serv" // &
   "ices (CCSDS 850.0-G-2). Available at: https://public.ccsds.org/Pubs/850x0g2.pdf (Accesse" // &
   "d: 21 November 2026).", &
  "Consultative Committee for Space Data Systems (2013b). Spacecraft Onboard Interface Syst" // &
   "ems" // &
   achar(226)//achar(128)//achar(148) // &
   "Low Data-Rate Wireless Communications for Spacecraft Monitoring and Control (CCSDS 882.0" // &
   "-M-1). Available at: https://public.ccsds.org/Pubs/882x0m1.pdf (Accessed: 21 November 20" // &
   "26).", &
  "Consultative Committee for Space Data Systems (2017a). Spacecraft Onboard Interface Serv" // &
   "ices" // &
   achar(226)//achar(128)//achar(148) // &
   "RFID Tag Encoding Specification (CCSDS 881.1-B-1). Available at: https://public.ccsds.or" // &
   "g/Pubs/881x1b1.pdf (Accessed: 21 November 2026).", &
  "Consultative Committee for Space Data Systems (2017b). Wireless Network Communications O" // &
   "verview for Space Mission Operations (CCSDS 880.0-G-3). Available at: https://public.ccs" // &
   "ds.org/Pubs/880x0g3.pdf (Accessed: 21 November 2026).", &
  "Consultative Committee for Space Data Systems (2018a). Data Entity Dictionary Specificat" // &
   "ion Language (DEDSL)" // &
   achar(226)//achar(128)//achar(148) // &
   "XML/XSD Syntax (CCSDS 647.4-O-1). Available at: https://public.ccsds.org/Pubs/647x4o1.pd" // &
   "f (Accessed: 21 November 2026).", &
  "Consultative Committee for Space Data Systems (2018b). Mission Operations" // &
   achar(226)//achar(128)//achar(148) // &
   "Message Abstraction Layer Binding to HTTP Transport and XML Encoding (CCSDS 524.3-B-1). " // &
   "Available at: https://public.ccsds.org/Pubs/524x3b1.pdf (Accessed: 21 November 2026).", &
  "Consultative Committee for Space Data Systems (2019). Spacecraft Onboard Interface Servi" // &
   "ces" // &
   achar(226)//achar(128)//achar(148) // &
   "XML Specification for Electronic Data Sheets (CCSDS 876.0-B-1). Available at: https://pu" // &
   "blic.ccsds.org/Pubs/876x0b1.pdf (Accessed: 21 November 2026).", &
  "Consultative Committee for Space Data Systems (2020). XML Telemetric and Command Exchang" // &
   "e" // &
   achar(226)//achar(128)//achar(148) // &
   "Version 1.2 (CCSDS 660.0-B-2). Available at: https://public.ccsds.org/Pubs/660x0b2.pdf (" // &
   "Accessed: 21 November 2026).", &
  "Consultative Committee for Space Data Systems (2021a). CAST Flight Software as a CCSDS O" // &
   "nboard Reference Architecture (CCSDS 811.1-O-1). Available at: https://public.ccsds.org/" // &
   "Pubs/811.1o1e1.pdf (Accessed: 21 November 2026).", &
  "Consultative Committee for Space Data Systems (2021b). MO Services and SOIS Electronic D" // &
   "atasheets (CCSDS 870.10-Y-1). Available at: https://public.ccsds.org/Pubs/870x10y1.pdf (" // &
   "Accessed: 21 November 2026).", &
  "Consultative Committee for Space Data Systems (2021c). XML Telemetric and Command Exchan" // &
   "ge (XTCE) (CCSDS 660.2-G-2). Available at: https://public.ccsds.org/Pubs/660x2g2.pdf (Ac" // &
   "cessed: 21 November 2026).", &
  "Consultative Committee for Space Data Systems (2021d). XML Telemetric and Command Exchan" // &
   "ge (XTCE)" // &
   achar(226)//achar(128)//achar(148) // &
   "Element Description (CCSDS 660.1-G-2). Available at: https://public.ccsds.org/Pubs/660x1" // &
   "g2.pdf (Accessed: 21 November 2026).", &
  "Consultative Committee for Space Data Systems (2023). XML Specification for Navigation D" // &
   "ata Messages (CCSDS 505.0-B-3). Available at: https://public.ccsds.org/Pubs/505x0b3e2.pd" // &
   "f (Accessed: 21 November 2026).", &
  "Consultative Committee for Space Data Systems (2024a). Electronic Data Sheets and Dictio" // &
   "nary of Terms for Onboard Devices and Components (CCSDS 870.0-G-1). Available at: https:" // &
   "//public.ccsds.org/Pubs/870x0g1.pdf (Accessed: 21 November 2026).", &
  "Consultative Committee for Space Data Systems (2024b). Spacecraft Onboard Interface Serv" // &
   "ices" // &
   achar(226)//achar(128)//achar(148) // &
   "High Data Rate Wireless Proximity Network Communications (CCSDS 883.0-B-2). Available at" // &
   ": https://public.ccsds.org/Pubs/883x0b2.pdf (Accessed: 21 November 2026).", &
  "Consultative Committee for Space Data Systems (2024c). Spacecraft Onboard Interface Serv" // &
   "ices" // &
   achar(226)//achar(128)//achar(148) // &
   "Specification for Dictionary of Terms for Electronic Data Sheets (CCSDS 876.1-M-1). Avai" // &
   "lable at: https://public.ccsds.org/Pubs/876x1m1.pdf (Accessed: 21 November 2026).", &
  "d3 (n.d.a). d3/d3-format [GitHub repository]. Available at: https://github.com/d3/d3-for" // &
   "mat (Accessed: 21 November 2026).", &
  "d3 (n.d.b). d3/d3-time-format [GitHub repository]. Available at: https://github.com/d3/d" // &
   "3-time-format (Accessed: 21 November 2026).", &
  "Debode, F., Huber, I., Macarthur, R., Rischitor, P.E., Mazzara, M., Herau, V., Sebah, D." // &
   ", Dobnik, D., Broeders, S., Roosens, N.H., Busch, U., Berben, G., Morisset, D. and Zel, " // &
   "J. (2017). 'Inter-laboratory studies for the validation of two singleplex (tE9 and pea l" // &
   "ectin) and one duplex (pat/bar) real-time PCR methods for GMO detection', Food Control, " // &
   "73, pp. 452-461. Available at: https://doi.org/10.1016/j.foodcont.2016.08.037 (Accessed:" // &
   " 21 November 2026).", &
  "European Cooperation for Space Standardization (2008). Space engineering: human factors " // &
   "engineering (ECSS-E-ST-10-11C, 31 July 2008). Available at: https://ecss.nl/standard/ecs" // &
   "s-e-st-10-11c-human-factors-engineering/ (Accessed: 21 November 2026).", &
  "fortran-lang (n.d.a). fortran-lang/minpack [GitHub repository]. Available at: https://gi" // &
   "thub.com/fortran-lang/minpack (Accessed: 21 November 2026).", &
  "fortran-lang (n.d.b). fortran-lang/stdlib [GitHub repository]. Available at: https://git" // &
   "hub.com/fortran-lang/stdlib (Accessed: 21 November 2026).", &
  "fortran-lang (n.d.c). fortran-lang/test-drive [GitHub repository]. Available at: https:/" // &
   "/github.com/fortran-lang/test-drive (Accessed: 21 November 2026).", &
  "fprime-community (n.d.). fprime-community/fprime-python-model [GitHub repository]. Avail" // &
   "able at: https://github.com/fprime-community/fprime-python-model (Accessed: 21 November " // &
   "2026).", &
  "Grohmann, L., Br" // &
   achar(195)//achar(188) // &
   "nen-Nieweler, C., Nemeth, A. and Waiblinger, H.U. (2009). 'Collaborative Trial Validatio" // &
   "n Studies of Real-Time PCR-Based GMO Screening Methods for Detection of the bar Gene and" // &
   " the ctp2-cp4epsps Construct', Journal of Agricultural and Food Chemistry, 57(19), pp. 8" // &
   "913-8920. Available at: https://doi.org/10.1021/jf901598r (Accessed: 21 November 2026).", &
  "Huff, V.N. and Strack, W.C. (1963). The N-body code: a general Fortran code for the nume" // &
   "rical solution of space mechanics problems on an IBM 7090 computer. Available at: https:" // &
   "//ntrs.nasa.gov/citations/19640000265 (Accessed: 21 November 2026).", &
  "jacobwilliams (n.d.a). jacobwilliams/dop853 [GitHub repository]. Available at: https://g" // &
   "ithub.com/jacobwilliams/dop853 (Accessed: 21 November 2026).", &
  "jacobwilliams (n.d.b). jacobwilliams/json-fortran [GitHub repository]. Available at: htt" // &
   "ps://github.com/jacobwilliams/json-fortran (Accessed: 21 November 2026).", &
  "Japan Aerospace Exploration Agency (2009). Design standard: spacecraft thermal control s" // &
   "ystem (JERG-2-310). Available at: https://sma.jaxa.jp/TechDoc/Docs/E_JAXA-JERG-2-310.pdf" // &
   " (Accessed: 21 November 2026).", &
  "Japan Aerospace Exploration Agency (2021). Software development standard for spacecraft " // &
   "(JERG-2-610B). Available at: https://sma.jaxa.jp/TechDoc/Docs/E_JAXA-JERG-2-610B.pdf (Ac" // &
   "cessed: 21 November 2026).", &
  "Jones, H.W. (2025). Common cause failures dominate and defeat redundancy (RAMS 2025, pre" // &
   "sentation). Available at: https://ntrs.nasa.gov/citations/20240013667 (Accessed: 21 Nove" // &
   "mber 2026).", &
  "Kalendar, R. (2025). 'Comprehensive web-based platform for advanced PCR design, genotypi" // &
   "ng, synthetic biology, molecular diagnostics, and sequence analysis', Molecular Therapy " // &
   "Nucleic Acids, 36(4), article 102716. Available at: https://doi.org/10.1016/j.omtn.2025." // &
   "102716 (Accessed: 21 November 2026).", &
  "kubo-azu (n.d.). kubo-azu/Click-qPCR [GitHub repository]. Available at: https://github.c" // &
   "om/kubo-azu/Click-qPCR (Accessed: 21 November 2026).", &
  "Kubota, A. and Tajima, A. (2025). 'Click-qPCR: A Simple Tool for Interactive qPCR Data A" // &
   "nalysis', BIO-PROTOCOL, 15(1384). Available at: https://doi.org/10.21769/bioprotoc.5513 " // &
   "(Accessed: 21 November 2026).", &
  "Lancraft, R.E. and Caglayan, A.K. (1983). FINDS: A fault inferring nonlinear detection s" // &
   "ystem. User's guide. Available at: https://ntrs.nasa.gov/citations/19840003088 (Accessed" // &
   ": 21 November 2026).", &
  "Lefever, S., Hellemans, J., Pattyn, F., Przybylski, D.R., Taylor, C., Geurts, R., Unterg" // &
   "asser, A., Vandesompele, J. and {on behalf of the RDML consortium} (2009). 'RDML: struct" // &
   "ured language and reporting guidelines for real-time quantitative PCR data', Nucleic Aci" // &
   "ds Research, 37(7), pp. 2065-2069. Available at: https://doi.org/10.1093/nar/gkp056 (Acc" // &
   "essed: 21 November 2026).", &
  "Lin, D., Crabtree, J., Dillo, I., Downs, R.R., Edmunds, R., Giaretta, D., De Giusti, M.," // &
   " L'Hours, H., Hugo, W., Jenkyns, R., Khodiyar, V., Martone, M.E., Mokrane, M., Navale, V" // &
   "., Petters, J., Sierman, B., Sokolova, D.V., Stockhause, M. and Westbrook, J. (2020). 'T" // &
   "he TRUST Principles for digital repositories', Scientific Data, 7(1), article 144. Avail" // &
   "able at: https://doi.org/10.1038/s41597-020-0486-7 (Accessed: 21 November 2026).", &
  "Livak, K.J. and Schmittgen, T.D. (2001). 'Analysis of Relative Gene Expression Data Usin" // &
   "g Real-Time Quantitative PCR and the 2" // &
   achar(226)//achar(136)//achar(146) // &
   achar(206)//achar(148) // &
   achar(206)//achar(148) // &
   "CT Method', Methods, 25(4), pp. 402-408. Available at: https://doi.org/10.1006/meth.2001" // &
   ".1262 (Accessed: 21 November 2026).", &
  "Mahmood-M-Ali (n.d.). Mahmood-M-Ali/qPCR-plate-planner [GitHub repository]. Available at" // &
   ": https://github.com/Mahmood-M-Ali/qPCR-plate-planner (Accessed: 21 November 2026).", &
  "Maussion, G., Thomas, R.A., Demirova, I., Gu, G., Cai, E., Chen, C.X.Q., Abdian, N., Str" // &
   "auss, T.J.P., Kela" // &
   achar(195)//achar(175) // &
   ", S., Nauleau-Javaudin, A., Beitel, L.K., Ramoz, N., Gorwood, P. and Durcan, T.M. (2021)" // &
   ". 'Auto-qPCR; a python-based web app for automated and reproducible analysis of qPCR dat" // &
   "a', Scientific Reports, 11(1), article 21293. Available at: https://doi.org/10.1038/s415" // &
   "98-021-99727-6 (Accessed: 21 November 2026).", &
  "mihaiionitabio-alt (n.d.a). mihaiionitabio-alt/lc480-ixo-qc-export [GitHub repository]. " // &
   "Available at: https://github.com/mihaiionitabio-alt/lc480-ixo-qc-export (Accessed: 21 No" // &
   "vember 2026).", &
  "mihaiionitabio-alt (n.d.b). mihaiionitabio-alt/lc480-ixo-qc-exportv2 [GitHub repository]" // &
   ". Available at: https://github.com/mihaiionitabio-alt/lc480-ixo-qc-exportv2 (Accessed: 2" // &
   "1 November 2026).", &
  "mihaiionitabio-alt (n.d.c). mihaiionitabio-alt/lc480-ixo-qc-kit [GitHub repository]. Ava" // &
   "ilable at: https://github.com/mihaiionitabio-alt/lc480-ixo-qc-kit (Accessed: 21 November" // &
   " 2026).", &
  "mihaiionitabio-alt (n.d.d). mihaiionitabio-alt/LIMS [GitHub repository]. Available at: h" // &
   "ttps://github.com/mihaiionitabio-alt/LIMS (Accessed: 21 November 2026).", &
  "mihaiionitabio-alt (n.d.e). mihaiionitabio-alt/RDMLT [GitHub repository]. Available at: " // &
   "https://github.com/mihaiionitabio-alt/RDMLT (Accessed: 21 November 2026).", &
  "mihaiionitabio-alt (n.d.f). mihaiionitabio-alt/VIRTUALPCR-MISSYR2 [GitHub repository]. A" // &
   "vailable at: https://github.com/mihaiionitabio-alt/VIRTUALPCR-MISSYR2 (Accessed: 21 Nove" // &
   "mber 2026).", &
  "mihaiionitabio-alt (n.d.g). mihaiionitabio-alt/zpcrweb [GitHub repository]. Available at" // &
   ": https://github.com/mihaiionitabio-alt/zpcrweb (Accessed: 21 November 2026).", &
  "mirzaghaderi (n.d.a). mirzaghaderi/rtpcr [GitHub repository]. Available at: https://gith" // &
   "ub.com/mirzaghaderi/rtpcr (Accessed: 21 November 2026).", &
  "mirzaghaderi (n.d.b). mirzaghaderi/shiny_rtpcr [GitHub repository]. Available at: https:" // &
   "//github.com/mirzaghaderi/shiny_rtpcr (Accessed: 21 November 2026).", &
  "nasa (n.d.). nasa/fprime [GitHub repository]. Available at: https://github.com/nasa/fpri" // &
   "me (Accessed: 21 November 2026).", &
  "NASA Jet Propulsion Laboratory (n.d.a). F Prime data products. Available at: https://fpr" // &
   "ime.jpl.nasa.gov/latest/docs/how-to/develop/data-products/ (Accessed: 21 November 2026).", &
  "NASA Jet Propulsion Laboratory (n.d.b). F Prime development practice. Available at: http" // &
   "s://fprime.jpl.nasa.gov/latest/docs/user-manual/overview/development-practice/ (Accessed" // &
   ": 21 November 2026).", &
  "NASA Jet Propulsion Laboratory (n.d.c). F Prime GDS command line interface. Available at" // &
   ": https://fprime.jpl.nasa.gov/latest/docs/user-manual/gds/gds-cli/ (Accessed: 21 Novembe" // &
   "r 2026).", &
  "NASA Jet Propulsion Laboratory (n.d.d). F Prime integration test API guide. Available at" // &
   ": https://fprime.jpl.nasa.gov/latest/docs/user-manual/gds/gds-test-api-guide/ (Accessed:" // &
   " 21 November 2026).", &
  "NASA Jet Propulsion Laboratory (n.d.e). FPP users guide. Available at: https://nasa.gith" // &
   "ub.io/fpp/fpp-users-guide.html (Accessed: 21 November 2026).", &
  "NASA Navigation and Ancillary Information Facility (2023). Writing a SPICE (Fortran) bas" // &
   "ed program (April 2023 tutorial). Available at: https://naif.jpl.nasa.gov/pub/naif/toolk" // &
   "it_docs/Tutorials/pdf/individual_docs/41_program_fortran.pdf (Accessed: 21 November 2026" // &
   ").", &
  "National Aeronautics and Space Administration (1975). FORTRAN Automated Code Evaluation " // &
   "System (faces) system documentation, version 2, mod 0. Available at: https://ntrs.nasa.g" // &
   "ov/citations/19750025673 (Accessed: 21 November 2026).", &
  "National Aeronautics and Space Administration (2012). Fault management handbook, Draft 2" // &
   ", 2 April 2012 (NASA-HDBK-1002 (draft)). Available at: https://s3vi.ndc.nasa.gov/ssri-kb" // &
   "/static/resources/636372main_NASA-HDBK-1002_Draft.pdf (Accessed: 21 November 2026).", &
  "National Aeronautics and Space Administration (2017). Human-rating requirements for spac" // &
   "e systems, updated with Change 2 (NPR 8705.2C). Available at: https://nodis3.gsfc.nasa.g" // &
   "ov/npg_img/N_PR_8705_002C_/N_PR_8705_002C_.pdf (Accessed: 21 November 2026).", &
  "National Aeronautics and Space Administration (2026). NASA Spaceflight Human-System Stan" // &
   "dard, Volume 2: Human Factors, Habitability, and Environmental Health (NASA-STD-3001, Vo" // &
   "lume 2, Revision F). Available at: https://www.nasa.gov/wp-content/uploads/2025/05/nasa-" // &
   "std-3001-vol-2.pdf (Accessed: 21 November 2026).", &
  "National Space Science Data Center (1998). CDF Fortran reference manual, version 2.6 (Ve" // &
   "rsion 2.6, 1 October 1998).", &
  "neuroeddu (n.d.). neuroeddu/Auto-qPCR [GitHub repository]. Available at: https://github." // &
   "com/neuroeddu/Auto-qPCR (Accessed: 21 November 2026).", &
  "Nguyen, D.D., Owen, J.W., Smith, D.A. and Lewter, W.J. (1994). A programmable heater con" // &
   "trol circuit for spacecraft. Available at: https://ntrs.nasa.gov/citations/19950004744 (" // &
   "Accessed: 21 November 2026).", &
  "notofonts (n.d.). notofonts/noto-cjk [GitHub repository]. Available at: https://github.c" // &
   "om/notofonts/noto-cjk (Accessed: 21 November 2026).", &
  "PCRuniversum (n.d.). PCRuniversum/RDML [GitHub repository]. Available at: https://github" // &
   ".com/PCRuniversum/RDML (Accessed: 21 November 2026).", &
  "Pfaffl, M.W. (2001). 'A new mathematical model for relative quantification in real-time " // &
   "RT-PCR', Nucleic Acids Research, 29(9), article 45e-45. Available at: https://doi.org/10" // &
   ".1093/nar/29.9.e45 (Accessed: 21 November 2026).", &
  "plotly (n.d.). plotly/plotly.js [GitHub repository]. Available at: https://github.com/pl" // &
   "otly/plotly.js (Accessed: 21 November 2026).", &
  "RByers (n.d.). RByers/zpcrweb [GitHub repository]. Available at: https://github.com/RBye" // &
   "rs/zpcrweb (Accessed: 21 November 2026).", &
  "RDML-consortium (n.d.). RDML-consortium/rdmlpython [GitHub repository]. Available at: ht" // &
   "tps://github.com/RDML-consortium/rdmlpython (Accessed: 21 November 2026).", &
  "rkalendar (n.d.). rkalendar/PCRtools [GitHub repository]. Available at: https://github.c" // &
   "om/rkalendar/PCRtools (Accessed: 21 November 2026).", &
  "Salazar, G.A. (2023). Establishing trust in NASA's Artemis campaign computer-human inter" // &
   "face (CHI) implementation (Revision D). Available at: https://ntrs.nasa.gov/citations/20" // &
   "230000983 (Accessed: 21 November 2026).", &
  "shellygil (n.d.). ShellyGil/PCR-Calculator [GitHub repository]. Available at: https://gi" // &
   "thub.com/ShellyGil/PCR-Calculator (Accessed: 21 November 2026).", &
  "Shelton, K.J., Arthur, J.J.I., Prinzel, L.J.I., Nicholas, S.N., Williams, S.P. and Baile" // &
   "y, R.E. (2015). Flight Test of a Head-Worn Display as an Equivalent-HUD for Terminal Ope" // &
   "rations. Available at: https://ntrs.nasa.gov/citations/20150010973 (Accessed: 21 Novembe" // &
   "r 2026).", &
  "Thurston, E. (2026). Development of the METMAN Thermal Desktop human model for system in" // &
   "tegration (ICES-2026-465).", &
  "Torres-Pomales, W. (2000). Software Fault Tolerance: A Tutorial. Available at: https://n" // &
   "trs.nasa.gov/citations/20000120144 (Accessed: 21 November 2026).", &
  "Untergasser, A., Hellemans, J., Pfaffl, M.W., Ruijter, J.M., van den Hoff, M.J.B., Drago" // &
   "mir, M.P., Adamoski, D., Dias, S.M.G., Reis, R.M., Ferracin, M., Dias" // &
   achar(226)//achar(128)//achar(144) // &
   "Neto, E., Marsh, I., Kubista, M., Fabbri, M., Goel, A., Slab" // &
   achar(195)//achar(189) // &
   ", O., Knutsen, E., Chen, B., Negrini, M., Mimori, K., Pichler, M., Papatriantafyllou, M." // &
   ", Anfossi, S., Schmittgen, T.D., Huggett, J., Bustin, S., Vandesompele, J., Calin, G.A. " // &
   "and {for the HEROIC (tHe initiativE gRoup On qRT dIsClosure) Consortium} (2023). 'Disclo" // &
   "sing quantitative RT" // &
   achar(226)//achar(128)//achar(144) // &
   "PCR raw data during manuscript submission: a call for action', Molecular Oncology, 17(5)" // &
   ", pp. 713-717. Available at: https://doi.org/10.1002/1878-0261.13418 (Accessed: 21 Novem" // &
   "ber 2026).", &
  "Vandesompele, J., De Preter, K., Pattyn, F., Poppe, B., Van Roy, N., De Paepe, A. and Sp" // &
   "eleman, F. (2002). 'Accurate normalization of real-time quantitative RT-PCR data by geom" // &
   "etric averaging of multiple internal control genes', Genome Biology, 3(7), article resea" // &
   "rch0034.1. Available at: https://doi.org/10.1186/gb-2002-3-7-research0034 (Accessed: 21 " // &
   "November 2026).", &
  "Waiblinger, H.U., Ernst, B., Anderson, A. and Pietsch, K. (2007). 'Validation and collab" // &
   "orative study of a P35S and T-nos duplex real-time PCR screening method to detect geneti" // &
   "cally modified organisms in food products', European Food Research and Technology, 226(5" // &
   "), pp. 1221-1228. Available at: https://doi.org/10.1007/s00217-007-0748-z (Accessed: 21 " // &
   "November 2026).", &
  "WebAudio (n.d.). WebAudio/web-speech-api [GitHub repository]. Available at: https://gith" // &
   "ub.com/WebAudio/web-speech-api (Accessed: 21 November 2026).", &
  "Weighardt, F., Barbati, C., Paoletti, C., Querci, M., Kay, S., De Beuckeleer, M. and Van" // &
   " den Eede, G. (2004). 'Real-Time Polymerase Chain Reaction-Based Approach for Quantifica" // &
   "tion of the pat Gene in the T25 Zea mays Event', Journal of AOAC INTERNATIONAL, 87(6), p" // &
   "p. 1342-1355. Available at: https://doi.org/10.1093/jaoac/87.6.1342 (Accessed: 21 Novemb" // &
   "er 2026).", &
  "Wenig, P. and Odermatt, J. (2010). 'OpenChrom: a cross-platform open source software for" // &
   " the mass spectrometric analysis of chromatographic data', BMC Bioinformatics, 11(1), ar" // &
   "ticle 405. Available at: https://doi.org/10.1186/1471-2105-11-405 (Accessed: 21 November" // &
   " 2026).", &
  "Wilkinson, M.D., Dumontier, M., Aalbersberg, I.J., Appleton, G., Axton, M., Baak, A., Bl" // &
   "omberg, N., Boiten, J.W., da Silva Santos, L.B., Bourne, P.E., Bouwman, J., Brookes, A.J" // &
   "., Clark, T., Crosas, M., Dillo, I., Dumon, O., Edmunds, S., Evelo, C.T., Finkers, R., G" // &
   "onzalez-Beltran, A., Gray, A.J.G., Groth, P., Goble, C., Grethe, J.S., Heringa, J., 't H" // &
   "oen, P.A.C., Hooft, R., Kuhn, T., Kok, R., Kok, J., Lusher, S.J., Martone, M.E., Mons, A" // &
   "., Packer, A.L., Persson, B., Rocca-Serra, P., Roos, M., van Schaik, R., Sansone, S.A., " // &
   "Schultes, E., Sengstag, T., Slater, T., Strawn, G., Swertz, M.A., Thompson, M., van der " // &
   "Lei, J., van Mulligen, E., Velterop, J., Waagmeester, A., Wittenburg, P., Wolstencroft, " // &
   "K., Zhao, J. and Mons, B. (2016). 'The FAIR Guiding Principles for scientific data manag" // &
   "ement and stewardship', Scientific Data, 3(1), article 160018. Available at: https://doi" // &
   ".org/10.1038/sdata.2016.18 (Accessed: 21 November 2026).", &
  "Yamaguchi, S.P., Itakura, R., Inoue, S. and Otsuka, A. (2025). Common human-robotic mani" // &
   "pulation interface: lessons learned from the ground robotic manipulation tests imitating" // &
   " ISS internal environments (ICES-2025-333).", &
  "fortran-lang (n.d.b). fortran-lang/stdlib [GitHub repository]. Used for: mean and varian" // &
   "ce in the temperature statistics of every RDMLt export and in the startup checks (browse" // &
   "r subset stdlib_browser_stats). Licence: MIT.", &
  "jacobwilliams (n.d.b). jacobwilliams/json-fortran [GitHub repository]. Used for: JSON st" // &
   "ring escaping in the profile and provenance exports (browser subset json_fortran_browser" // &
   "_strings). Licence: MIT-style.", &
  "Lefever, S. et al. (2009). RDML: structured language and reporting guidelines for real-t" // &
   "ime quantitative PCR data. With the RDML consortium RDML 1.3 schema. Used for: the rdml_" // &
   "data.xml core (dyes, samples, targets, thermal cycling conditions, reactions, amplificat" // &
   "ion points); every export is validated against RDML 1.3 in the test suite.", &
  "mihaiionitabio-alt (n.d.e). mihaiionitabio-alt/RDMLT [GitHub repository]. RDMLt 1.0-draf" // &
   "t companion schema. Used for: rdmlt_data.xml (files with SHA-256, dictionary, instrument" // &
   " sensors and channels, conditions, temperature logs, series, acquisitions, events, analy" // &
   "ses, reactions with signals and results).", &
  "mihaiionitabio-alt (n.d.f). mihaiionitabio-alt/VIRTUALPCR-MISSYR2 [GitHub repository]. U" // &
   "sed for: the package layout reproduced by this edition (telemetry, acquisitions, simulat" // &
   "or record, run input, chemistry, evaluation, assay detail, temperature statistics), the " // &
   "startup screen design, and the origin of the thermal, chemistry and optics models ported" // &
   " to Fortran.", &
  "National Institute of Standards and Technology (2015). Secure Hash Standard (SHS) (FIPS " // &
   "PUB 180-4). Available at: https://doi.org/10.6028/NIST.FIPS.180-4 (Accessed: 23 November" // &
   " 2026). Used for: SHA-256 digests of every manifest entry (native_codec).", &
  "Deutsch, P. (1996). DEFLATE Compressed Data Format Specification version 1.3 (RFC 1951)." // &
   " Available at: https://doi.org/10.17487/RFC1951 (Accessed: 23 November 2026). Used for: " // &
   "compressed ZIP entries (fixed Huffman codes with LZ77 matching, native_codec).", &
  "PKWARE Inc. (n.d.). APPNOTE.TXT - .ZIP File Format Specification. Available at: https://" // &
   "pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT (Accessed: 23 November 2026). Used f" // &
   "or: the ZIP container of .rdmlt packages (local headers, central directory, CRC-32).", &
  "notofonts (n.d.). notofonts/noto-cjk [GitHub repository]. Used for: the two-bit bitmap f" // &
   "ont of the Fortran renderer (Noto Sans CJK SC, SIL OFL 1.1), including the glyphs added " // &
   "for this page.", &
  "lfortran (n.d.). lfortran/lfortran [GitHub repository]. Available at: https://github.com" // &
   "/lfortran/lfortran (Accessed: 23 November 2026). Used for: compiling the Fortran kernel " // &
   "to WebAssembly (LFortran 0.69).", &
  "emscripten-core (n.d.). emscripten-core/emscripten [GitHub repository]. Available at: ht" // &
   "tps://github.com/emscripten-core/emscripten (Accessed: 23 November 2026). Used for: link" // &
   "ing the WebAssembly module and its browser runtime.", &
  "Mullins, E.A., Salay, L.E., Durie, C.L., Bradley, N.P., Jackman, J.E., Ohi, M.D., Chazin" // &
   ", W.J. and Eichman, B.F. (2024). A mechanistic model of primer synthesis from catalytic " // &
   "structures of DNA polymerase alpha-primase. Nature Structural & Molecular Biology, 31, 7" // &
   "77. Available at: https://doi.org/10.1038/s41594-024-01227-4 (Accessed: 24 November 2026" // &
   "). Used for: the rotating polymerase-DNA structure view (PDB entry 8UCU, https://doi.org" // &
   "/10.2210/pdb8UCU/pdb, wwPDB data CC0 1.0): C-alpha trace, template, primer, incoming dGT" // &
   "P, ddC and Mg2+ (native_structure).", &
  "Joshi, M. (2025). ProteinDNAInterfaceAnalysis [GitHub repository]. Licence: MIT. Used fo" // &
   "r: the DNA-contact residues of the 8UCU polymerase (Naccess .int output distributed with" // &
   " the repository), highlighted in the structure view.", &
  "Hoover, D.M. and Lubkowski, J. (2002). DNAWorks: an automated method for designing oligo" // &
   "nucleotides for PCR-based gene synthesis. Nucleic Acids Research, 30(10), e43. Used for:" // &
   " an independent check of the nearest-neighbour enthalpy table used for the example prime" // &
   "r Tm shown in the annealing scenes (no DNAWorks code is included; DNAWorks is GPL-2.0).", &
  "SantaLucia, J. Jr. (1998). A unified view of polymer, dumbbell, and oligonucleotide DNA " // &
   "nearest-neighbor thermodynamics. Proceedings of the National Academy of Sciences USA, 95" // &
   "(4), 1460-1465. Available at: https://doi.org/10.1073/pnas.95.4.1460 (Accessed: 24 Novem" // &
   "ber 2026). Used for: nearest-neighbour duplex thermodynamics in r2_tm_nn.", &
  "Owczarzy, R., Moreira, B.G., You, Y., Behlke, M.A. and Walder, J.A. (2008). Predicting s" // &
   "tability of DNA duplexes in solutions containing magnesium and monovalent cations. Bioch" // &
   "emistry, 47(19), 5336-5353. Available at: https://doi.org/10.1021/bi702363u (Accessed: 2" // &
   "4 November 2026). Used for: the salt and magnesium correction in r2_tm_nn.", &
  "Bio-Rad Laboratories (n.d.). iCycler iQ Real-Time PCR Detection System Instruction Manua" // &
   "l, catalog number 170-8740 (document 4006200 Rev E). Used for: the layout of the instrum" // &
   "ent cross-section in the scenario animations (light source, excitation filters, mirrors," // &
   " sample plate, emission filters, intensifier and CCD detector), adapted to the simulated" // &
   " MISSY R2 instrument.", &
  "Retained: native generic DNA probe assay, 45 cycles, 1-3 extractions and replicates; nat" // &
   "ive kinetics, thermal plant and PID controller, dual-sensor monitor, raw integer fluores" // &
   "cence; batches of 1-20 runs with seeded variation and teaching control review.", &
  "qPCR scenarios: tick any number of cards (and the normal run) and set how many runs each" // &
   "; Run selection files them one after another, up to the 32-run history. Opening a card s" // &
   "hows an animation of the mechanism (instrument cross-section, molecules in the well, wel" // &
   "ls and plate map, data path, or the 8UCU structure) and the fault curves against the nor" // &
   "mal reference, both simulated with the full model, same seed and profile. The animations" // &
   " illustrate the mechanism; they are not measurements.", &
  "Exports: full RDMLt packages with the MISSY R2 layout (RDML 1.3 core, RDMLt companion wi" // &
   "th SHA-256, telemetry, acquisitions, simulator record, chemistry snapshots, evaluation, " // &
   "assay detail and temperature statistics). Packages open in MISSY. Imports: this edition'" // &
   "s own packages and profiles. CRC-32 and SHA-256 detect corruption; there is no authentic" // &
   "ation.", &
  "Not included: legacy instrument readers, MARA and the advanced SOP editor, SOP evaluatio" // &
   "n profiles, quizzes, the three builders, checkpoint recovery and the bilingual interface" // &
   ".", &
  "Controls: pointer and keyboard. Escape returns Home; Space stops and files the current r" // &
   "un. Numeric fields: click a value, type, then Enter to apply; Escape cancels. Charts: wh" // &
   "eel to zoom, drag to pan.", &
  "All data are simulated. Research use only; not validated; not a conformity or validation" // &
   " claim." ]
end module
