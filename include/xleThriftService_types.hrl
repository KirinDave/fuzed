-ifndef(_xleThriftService_types_included).
-define(_xleThriftService_types_included, yeah).

-record(xleThriftServiceException, {what}).

-record(xleThriftNlMatchData, {match, score, alignments}).

-record(optionValues, {toggleValues, keyedValues}).

-record(retrievalQel, {selection, scoring, aggregation}).

-record(summarizationQel, {keywordSelection, keywordScoring, semanticSelection, semanticScoring, semanticPartitionSpecs}).

-record(xleThriftQel, {retrievalQel, summarizationQel, semrep, planOptions}).

-record(xleThriftParseResponse, {qel, properties}).

-endif.
