-- Hook early into the loading phase before network initialization
mp.add_hook("on_load", 50, function()
    local path = mp.get_property("path", "")
    
    -- Intercept the browser address bar watch link
    if path:find("192%.168%.1%.236:3000/watch%?v=") then
        local video_id = path:match("v=([^&]+)")
        if video_id then
            mp.msg.info("Querying Invidious API with jq filter...")
            
            -- Halt the rigid loader loop so mpv doesn't panic
            mp.set_property("path", "")
            
            local api_url = "http://192.168.1.236:3000/api/v1/videos/" .. video_id
            
            -- Single network call: jq extracts the title on line 1, followed by all available itags below it
            local shell_command = "curl -s '" .. api_url .. "' | jq -r '.title, .adaptiveFormats[].itag, .formatStreams[].itag' 2>/dev/null"
            
            mp.command_native_async({
                name = "subprocess",
                capture_stdout = true,
                args = { "sh", "-c", shell_command }
            }, function(success, res)
                if not success or not res.stdout or res.stdout == "" then
                    mp.msg.error("Invidious API or jq parsing failed.")
                    return
                end
                
                -- Extract the title (which is always the very first line of the jq output)
                local first_newline = res.stdout:find("\n")
                local video_title = ""
                local itag_payload = res.stdout
                
                if first_newline then
                    video_title = res.stdout:sub(1, first_newline - 1)
                    itag_payload = res.stdout:sub(first_newline + 1)
                end
                
                -- Ordered preferences: 1080p60 VP9, 1080p30 VP9, 1080p60 H264, 1080p30 H264, 720p60 VP9, 720p30 VP9
                local target_itags = { "303", "248", "299", "137", "302", "247" }
                local chosen_itag = nil
                
                -- Check the clean list returned by jq for our preferred formats
                for _, itag in ipairs(target_itags) do
                    if itag_payload:find(itag) then
                        chosen_itag = itag
                        break
                    end
                end
                
                -- Absolute emergency fallback if the video is ancient and has no HD adaptive formats
                if not chosen_itag then
                    chosen_itag = "18" 
                    mp.msg.warn("No preferred HD tracks found. Using fallback progressive track.")
                else
                    mp.msg.info("Matched Available Format! Chosen itag: " .. chosen_itag)
                end
                
                local video_track = "http://192.168.1.236:3000/latest_version?id=" .. video_id .. "&local=true&itag=" .. chosen_itag
                local audio_track = "http://192.168.1.236:3000/latest_version?id=" .. video_id .. "&local=true&itag=251"
                
                -- Handle the audio track injection right when the file finishes loading
                local function inject_audio()
                    mp.commandv("audio-add", audio_track, "auto", "External Audio")
                    mp.set_property("aid", "1")
                    mp.unregister_event(inject_audio)
                end
                mp.register_event("file-loaded", inject_audio)
                
                -- Safely launch the validated track and bind options using a native table dictionary.
                -- This protects commas and quotes inside the title string from breaking the parameters array.
                if video_title ~= "" then
                    mp.command_native({
                        name = "loadfile",
                        url = video_track,
                        flags = "replace",
                        options = {
                            ["demuxer-lavf-format"] = "matroska,webm",
                            ["ytdl"] = "no",
                            ["force-media-title"] = video_title,
                            ["title"] = video_title
                        }
                    })
                else
                    mp.command_native({
                        name = "loadfile",
                        url = video_track,
                        flags = "replace",
                        options = {
                            ["demuxer-lavf-format"] = "matroska,webm",
                            ["ytdl"] = "no"
                        }
                    })
                end
            end)
        end
    end
end)
