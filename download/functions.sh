#! /usr/bin/env bash
# Functions

get_result_count() {
	tail -c 250 $1 | grep -oP '\"result_count\":[0-9]{1,}' | sed 's/\"result_count\"://g'
}

get_oldest_id () {
	tail -c 250 $1 | grep -oP '\"oldest_id\":\"(.+?)\"' | sed 's/oldest_id//g;s/[\":]//g'
}

get_sample_return_date () {
	head -c 5000 $1 | grep -oP '\"created_at\":\"(.+?)T' | sed 's/created_at//g;s/[\":T]//g' | head -n 1
}

encode_hashtag () {
	echo $1 | sed 's/#/%23/' | sed 's/ /%20/g'
}

decode_hashtag () {
	echo $1 | sed 's/%23/#/' | sed 's/%20/ /g'
}

update_filename () {
	filename="json/$(echo "$q" | sed 's/%23/#/' | sed 's/ /+/g' | sed 's/%20/+/g')-$count.json"
}

update_initial_query () {
	q=$(encode_hashtag "$q")
	query=$(echo "?query=$q&max_results=$max&start_time=$from&end_time=$to$vars" | sed 's/[\r\n]//g')
}

update_query () {
	query="?query=$q&max_results=$max&until_id=$until$vars"
}

send_url_save_to () {
curl \
	-k \
    --connect-timeout 30 \
    --retry 100 \
    --retry-delay 8 \
	--retry-connrefused \
    "https://api.twitter.com/2/tweets/search/all$1" -H "Authorization: Bearer $BEARER_TOKEN" > $2
}

download_current_q () {
printf "\n\n*** Downloading %s ***\n\n" "$(decode_hashtag "$q")"
let count=1
update_filename
update_initial_query
while [[ true ]]
do	
	SECONDS=0  # every loop should take at least 4 seconds (rate limit)
	send_url_save_to $query $filename
	if ! [[ $(get_result_count $filename) -gt 0 ]]
	then
		rm $filename
		printf "\n\n*** Download $(decode_hashtag $q) complete ***\n\n"
		break
	fi
	printf "\n\n*** Just downloaded $(decode_hashtag "$q"). If found, reference date is %s ***\n\n" $(get_sample_return_date $filename)
	until=$(get_oldest_id $filename)
	let count+=1
	update_query
	update_filename
	while ! [[ $SECONDS -ge 4 ]]; do sleep 0.1; done 
done
}

main() {
    printf "\n*** Functions Imported Successfully ***\n"
}

if [ "${1}" != "--source-only" ]
then
    main "${@}"
fi
