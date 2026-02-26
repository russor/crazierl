"use strict";

document.getElementById("dist_consent").checked = false;

function generate_subkey(hashkey, extra, f) {
    const enc = new TextEncoder();
    const extrastr = enc.encode(extra);

    var extra_bits;
    
    window.crypto.subtle.importKey(
        "raw",
        extrastr,
        "PBKDF2",
        false,
        ["deriveBits"],
    ).then(function (extra_key) {
        return window.crypto.subtle.deriveBits( 
        {
          name: "PBKDF2",
          salt: hashkey,
          iterations: 10000,
          hash: { name: "SHA-256" },
        },
        extra_key,
        256);
    }).then(function (extra_bits) {
        var bytes = new Uint8Array(extra_bits);
        f(bytes);
    });
}

window.onload = function()
{
    var hashkey = null;
    if (window.location.hash != "") {
        try {
            hashkey = Uint8Array.fromBase64(window.location.hash.substr(1), { alphabet: "base64url"});
        } catch (e) {
            hashkey = null;
        }
    }
    
    if (hashkey == null || hashkey.byteLength < 16) {
        hashkey = new Uint8Array(16);
        self.crypto.getRandomValues(hashkey);
    }
    window.location.hash = "#" + hashkey.toBase64({ alphabet: "base64url", omitPadding: true});
    document.getElementById("hash").value = hashkey.toBase64({ alphabet: "base64url", omitPadding: true});
    document.getElementById("dist_consent").on
    
 
    var emulator = new V86({
        wasm_path: "v86.wasm",
        memory_size: 128 * 1024 * 1024,
        vga_memory_size: 2 * 1024 * 1024,
        serial_container_xtermjs: document.getElementById("terminal"),
        bios: {
            url: "seabios.bin",
        },
        vga_bios: {
            url: "vgabios.bin",
        },
        multiboot: {
            url: "crazierl.elf",
        },
        initrd: {
            url: "initrd",
        },
        cmdline: "kernel /libexec/ld-elf32.so.1",
        autostart: true,
        acpi: true,
        disable_speaker: true,
        log_level: 0,
        uart1: true,
        uart2: false,
        net_device: {
            type: "virtio",
            relay_url: "wss://relay.widgetry.org/",
        },
    });
    
    var buffer = null;
    var buffer_pos = 0;
    var buffer_function;
    var buffer_size = 0;
    
    var ws;
    
    function node_name() {
        const utf8decoder = new TextDecoder();
        var nodename = utf8decoder.decode(buffer);
        console.log("node name is", nodename);
        
        generate_subkey(hashkey, "websocket", function (bytes) {
                ws = new WebSocket("wss://gather.crazierl.net/ws?node=" + encodeURIComponent(nodename) + "&key=" + bytes.toHex());
                ws.binaryType = "arraybuffer";
                ws.onmessage = function(m) {
                    var data = new Uint8Array(m.data);
                    if (data[0] == 0x6e) {
                        var length = data.byteLength - 1;
                        emulator.serial_send_bytes(1, [0x6e, length >> 8, length & 0xFF]);
                        emulator.serial_send_bytes(1, data.slice(1));
                        console.log("sent node");
                    } else {
                        console.log("unknown command from websocket", data[0], data);
                    }
                }
        });
    }
    
    
    function crazierl_byte_in(b) {
        if (buffer != null) {
            buffer[buffer_pos] = b;
            ++buffer_pos;
            if (buffer_pos == buffer.byteLength) {
                buffer_function(buffer);
                buffer = null;
                buffer_function = null;
                buffer_pos = 0;
                buffer_size = 0;
            }
        } else if (buffer_function != null) {
            buffer_size |= b;
            --buffer_pos;
            if (buffer_pos > 0) {
                buffer_size <<= 8;
            } else {
                buffer = new Uint8Array(b);
            }
        } else if (b == 0x3f) { // ?
            emulator.serial_send_bytes(1, [0x21]); // !
            maybe_send_cookie();
            document.getElementById("dist_consent").onchange =  maybe_send_cookie;
        } else if (b == 0x6e) { // n
            buffer = null;
            buffer_function = node_name;
            buffer_pos = 2;
        } else {
            console.log("got byte from crazierl: " + b);
        }
    }

    emulator.add_listener("serial1-output-byte", function(byte) {
        crazierl_byte_in(byte);
    }); 


    function maybe_send_cookie() {
        if (document.getElementById("dist_consent").checked) {
            document.getElementById("dist_consent").disabled = true;
            generate_subkey(hashkey, "cookie", function (bytes) {
                    emulator.serial_send_bytes(1, [0x63, 0, bytes.byteLength]); // c, size
                    emulator.serial_send_bytes(1, bytes);
            });
        }
    }
    
}
