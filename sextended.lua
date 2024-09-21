if not ffi then ffi = require 'ffi' end
if not bit then bit = require 'bit' end

ffi.cdef[[
    typedef struct
    {
        float x;
        float y;
        float z;
    } vec3_t;

    typedef struct
    {
        float x;
        float y;
    } vec2_t;
]]

local bUnloaded = false
local Signatures = {}

local User = {}
while true do
    if not _G then
        if not quick_maths then
            if not info then
                if not info.fatality then
                    User.Cheat = 'evolve'
                    break
                end
            end
            User.Cheat = 'fatality'
            break
        end
        User.Cheat = 'rifk7'
        break
    end
    if MatSystem then
        User.Cheat = 'spirthack'
        break
    end
    if file then
        User.Cheat = 'legendware'
        break
    end
    if GameEventManager then
        User.Cheat = 'memesense'
        break
    end
    if penetration then
        User.Cheat = 'pandora'
        break
    end
    if math_utils then
        User.Cheat = 'legion'
        break
    end
    if plist then
        User.Cheat = 'gamesense'
        break
    end
    if network then
        User.Cheat = 'neverlose'
        break
    end
    if renderer and renderer.setup_texture then
        User.Cheat = 'nixware'
        break
    end
    User.Cheat = 'primordial'
    break
end
local bIsffiCCompatible = ffi.C and User.Cheat ~= 'gamesense'

local WorldToScreen = function (...)
    --return render.world_to_screen(...)
    return {
        x = 0,
        y = 0
    }
end

(function () -- math extended
    math.round = function (num)
        return math.floor(num + 0.5)
    end

    math.deg2rad = function (x)
        return x * (math.pi / 180)
    end

    math.lerp = function(a, b, t)
        return a + (b - a) * t
    end

    math.mapnumber = function(input, input_min, input_max, output_min, output_max)
        return (input - input_min) / (input_max - input_min) * (output_max - output_min) + output_min
    end

    math.clamp = function(v, min, max)
        if min > max then
            min, max = max, min
        end
        if v < min then
            return min
        end
        if v > max then
            return max
        end
        return v
    end

    
end)()

local vec2_t, vec3_t, vector_data_t = (function () -- Vectors
    local vector_data_t = {
        x = 0,
        y = 0,
        z = nil,
        type = ''
    }
    
    function vector_data_t:new(o)
        o = o or {}
        setmetatable(o, self)
        self.__index = self
        return o
    end

    local vec2_t = function (x, y, z)
        if type(x) ~= 'number' and x ~= nil then error('[S-API](vec2_t) Invalid argument .x (must be a number)') end
        if type(y) ~= 'number' and y ~= nil then error('[S-API](vec2_t) Invalid argument .y (must be a number)') end

        if z and type(z) == "number" then
            local vec2 = WorldToScreen(x, y, z)
            x = vec2.x
            y = vec2.y
        end

        return vector_data_t:new({
            x = x or 0,
            y = y or 0,
            type = 'vec2_t'
        })
    end

    local vec3_t = function (x, y, z)
        if type(x) == "table" or x.x ~= nil then
            if x.z then
                z = x.z
            end
            if x.y then
                y = x.y
            end
            if x.x then
                x = x.x
            end
        end
        if type(x) ~= 'number' and x ~= nil then error('[S-API](vec3_t) Invalid argument .x (must be a number)') end
        if type(y) ~= 'number' and y ~= nil then error('[S-API](vec3_t) Invalid argument .y (must be a number)') end
        if type(z) ~= 'number' and z ~= nil then error('[S-API](vec3_t) Invalid argument .z (must be a number)') end

        return vector_data_t:new({
            x = x or 0,
            y = y or 0,
            z = z or 0,
            type = 'vec3_t'
        })
    end

    function vector_data_t:ffi()
        if self.type == 'vec3_t' then
            return ffi.new(ffi.typeof('vec3_t'), {x = self.x, y = self.y, z = self.z})
        end
        return ffi.new(ffi.typeof('vec2_t'), {x = self.x, y = self.y})
    end

    function vector_data_t:Add(i)
        if type(i) ~= 'number' and type(i) ~= 'table' then error('[S-API](' .. self.type .. ':Add) Invalid argument .i (must be a number)') end

        if type(i) == 'number' then
            return (self.type == "vec2_t" and vec2_t or vec3_t)(self.x + i, self.y + i, self.z + i)
        else
            return (self.type == "vec2_t" and vec2_t or vec3_t)(self.x + i.x, self.y + i.y, self.z + i.z)
        end
    end

    function vector_data_t:Sub(i)
        if type(i) == "number" then
            i = -i
        elseif type(i) == "table" then
            i.x = -i.x
            i.y = -i.y
            if i.z then i.z = -i.z end
        else error('[S-API](' .. self.type .. ':Sub) Invalid argument .i (must be a number)')
        end
        return self:Add(i)
    end

    function vector_data_t:Multiply(i)
        if type(i) ~= 'number' and type(i) ~= 'table' then error('[S-API](' .. self.type .. ':Multiply) Invalid argument .i (must be a number)') end

        if type(i) == 'number' then
            return (self.type == "vec2_t" and vec2_t or vec3_t)(self.x * i, self.y * i, self.z * i)
        else
            return (self.type == "vec2_t" and vec2_t or vec3_t)(self.x * i.x, self.y * i.y , self.z * i.z)
        end
    end

    function vector_data_t:Fraction(i)
        if type(i) ~= 'number' and type(i) ~= 'table' then error('[S-API](' .. self.type .. ':Fraction) Invalid argument .i (must be a number)') end

        if type(i) == 'number' then
            return (self.type == "vec2_t" and vec2_t or vec3_t)(self.x / i, self.y / i, self.z / i)
        else
            return (self.type == "vec2_t" and vec2_t or vec3_t)(self.x / i.x, self.y / i.y, self.z / i.z)
        end
    end

    function vector_data_t:Dot(vector)
        if type(vector) ~= 'table' then error('[S-API](' .. self.type .. ':Dot) Invalid argument .vector (must be a vec2_t or a vec3_t)') end
        return (self.x * vector.x + self.y * vector.y + (self.type == 'vector3d' and self.z * (vector.z or 1) or 0))
    end

    function vector_data_t:Length()
        return math.sqrt(self:Dot(self))
    end

    function vector_data_t:Length2d()
        return math.sqrt(self.x * self.x + self.y * self.y)
    end

    function vector_data_t:Distance(distance)
        distance = distance or 0
        if type(distance) ~= 'number' then error('[S-API](' .. self.type .. ':Distance) Invalid argument .distance (must be a number)') end
        return self:Sub(distance):Length()
    end

    function vector_data_t:Angle(vector)
        if type(vector) ~= 'table' then error('[S-API](' .. self.type .. ':Angle) Invalid argument .vector (must be a vec2_t or a vec3_t)') end
        return math.acos(self:Dot(vector) / (self:Length() + vector:Length()))
    end

    function vector_data_t:Cross(vector)
        if type(vector) ~= 'table' then error('[S-API](' .. self.type .. ':Cross) Invalid argument .vector (must be a vec3_t)') end
        if self.type == 'vec2_t' then error('[S-API](' .. self.type .. ':Cross) Invalid vector type (must be a vec3_t)') end
        if vector.type == 'vec2_t' then error('[S-API](' .. self.type .. ':Cross) Invalid vector type argument (must be a vec3_t)') end
        return vec3_t(self.y * vector.z - (self.z or 0) * vector.y, (self.z or 0) * vector.x - self.x * vector.z, self.x * vector.y - self.y * vector.x)
    end

    function vector_data_t:unpack()
        if self.type == 'vec3_t' then
            return self.x, self.y, self.z
        end
    
        return self.x, self.y
    end
    
    return vec2_t, vec3_t, vector_data_t
end)()

local HSV, HEX, Color = (function ()
    local hex_t = {
        v = '#FFFFFFFF',
        type = 'hex'
    }
    
    local hsv_t = {
        h = 0,
        s = 0,
        v = 100,
        type = 'hsv'
    }
    
    local color_t = {
        r = 255,
        g = 255,
        b = 255,
        a = 255,
        type = 'rgba'
    }
    
    function hex_t:new(o)
        o = o or {}
        setmetatable(o, self)
        self.__index = self
        return o
    end
    
    local HEX = function(hex)
        return hex_t:new({
            v = hex or '#FFFFFFFF'
        })
    end
    
    function hsv_t:new(o)
        o = o or {}
        setmetatable(o, self)
        self.__index = self
        return o
    end
    
    local HSV = function(h, s, v)
        return hsv_t:new({
            h = h or 0,
            s = s or 0,
            v = v or 0
        })
    end
    
    function color_t:new(o)
        o = o or {}
        setmetatable(o, self)
        self.__index = self
        return o
    end
    
    local Color = function(r, g, b, a)
        return color_t:new({
            r = math.clamp(math.floor(tonumber(r) or 255), 0, 255),
            g = math.clamp(math.floor(tonumber(g) or 255), 0, 255),
            b = math.clamp(math.floor(tonumber(b) or 255), 0, 255),
            a = math.clamp(math.floor(tonumber(a) or 255), 0, 255)
        })
    end
    
    function hex_t:Get()
        return self.v
    end
    
    function hex_t:toRGB()
        local hexs = self:Get()
        if string.find(hexs, "#") then
            local hex = hexs:gsub("#", "")
            return Color(tonumber("0x" .. hex:sub(1, 2)), tonumber("0x" .. hex:sub(3, 4)), tonumber("0x" .. hex:sub(5, 6)),
                tonumber("0x" .. hex:sub(7, 8)))
        else
            print('[S-API](hex_t:toRGB) Invalid Hex')
        end
        return Color()
    end
    
    function hsv_t:toRGB()
        local hue = self.h or 0
        local sat = self.s or 0
        local val = self.v or 0
    
        local red = 0.0
        local grn = 0.0
        local blu = 0.0
    
        local i = 0.0
        local f = 0.0
        local p = 0.0
        local q = 0.0
        local t = 0.0
    
        if val == 0 then
            red = 0.0;
            grn = 0.0;
            blu = 0.0;
        else
            hue = hue / 60.0;
            i = math.floor(hue)
            f = hue - i;
            p = val * (1.0 - sat);
            q = val * (1.0 - (sat * f));
            t = val * (1.0 - (sat * (1.0 - f)));
            if i == 0.0 then
                red = val;
                grn = t;
                blu = p;
            elseif i == 1.0 then
                red = q;
                grn = val;
                blu = p;
            elseif i == 2.0 then
                red = p;
                grn = val;
                blu = t;
            elseif i == 3.0 then
                red = p;
                grn = q;
                blu = val;
            elseif i == 4.0 then
                red = t;
                grn = p;
                blu = val;
            elseif i == 5.0 then
                red = val;
                grn = p;
                blu = q;
            end
        end
    
        return Color(math.ceil(red * 255.0), math.ceil(grn * 255.0), math.ceil(blu * 255.0), 255)
    end
    
    function color_t:toHSV()
        local hue = 0.0
        local sat = 0.0
        local val = 0.0
        local x = 0.0
        local f = 0.0
        local i = 0.0
    
        local r = 0.0
        local g = 0.0
        local b = 0.0
    
        r = self.r / 255.0
        g = self.g / 255.0
        b = self.b / 255.0
    
        x = math.min(math.min(r, g), b)
        val = math.max(math.max(r, g), b)
    
        if x == val then
            hue = 0.0
            sat = 0.0
        else
            if r == x then
                f = g - b
                i = 3.0
            elseif g == x then
                f = b - r
                i = 5.0
            else
                f = r - g
                i = 1.0
            end
    
            hue = math.fmod((i - f / (val - x)) * 60.0, 360.0)
            sat = ((val - x) / val)
        end
    
        return HSV(hue, sat, val)
    end
    
    function color_t:unpack()
        return self.r, self.g, self.b, self.a
    end
    
    function color_t:toHEX()
        local rgb = {self.r, self.g, self.b, self.a}
        local hexadecimal = '#'
    
        for key, value in pairs(rgb) do
            local hex = ''
    
            while (value > 0) do
                local index = math.fmod(value, 16) + 1
                value = math.floor(value / 16)
                hex = string.sub('0123456789ABCDEF', index, index) .. hex
            end
    
            if (string.len(hex) == 0) then
                hex = '00'
    
            elseif (string.len(hex) == 1) then
                hex = '0' .. hex
            end
    
            hexadecimal = hexadecimal .. hex
        end
    
        return hexadecimal
    end
    
    function color_t:SetAlpha(alpha)
        return Color(self.r, self.g, self.b, math.clamp(math.floor(alpha), 0, 255))
    end
    
    function color_t:Print(b)
        b = b or ' '
        return tostring(self.r .. b .. self.g .. b .. self.b .. b .. self.a)
    end
    
    function color_t:Lerp(new_color, fraction)
        return Color(self.r + (new_color.r - self.r) * fraction, self.g + (new_color.g - self.g) * fraction,
            self.b + (new_color.b - self.b) * fraction, self.a + (new_color.a - self.a) * fraction)
    end
    
    --[[
    function color_t:Rainbow()
        self.r = math.floor(math.sin(Engine.GetRealTime() * 1.0) * 127 + 128)
        self.g = math.floor(math.sin(Engine.GetRealTime() * 1.0 + 2) * 127 + 128)
        self.b = math.floor(math.sin(Engine.GetRealTime() * 1.0 + 4) * 127 + 128)
    
        return self
    end]]

    return HSV, HEX, Color
end)()

--[[
local UnloadFunction = function (func)
    bUnloaded = true
    client.add_callback("unload", func)
end]]

local OriginalUtils = Utils -- for cheats that uses uppercase Utils
local Utils = (function()
    local findsignaturefunc
    local createinterfacefunc
    local UnloadFunction

    if User.Cheat == "evolve" then
        findsignaturefunc = utils.find_pattern
        createinterfacefunc = utils.find_interface
        UnloadFunction = function(fn)

        end
    elseif User.Cheat == "fatality" then
        findsignaturefunc = utils.find_pattern
        createinterfacefunc = utils.find_interface
        UnloadFunction = function(fn)

        end
    elseif User.Cheat == "primordial" then
        findsignaturefunc = memory.find_pattern
        createinterfacefunc = memory.create_interface
        UnloadFunction = function(fn)
            return callbacks.add(e_callbacks.SHUTDOWN, fn)
        end
    elseif User.Cheat == "memesense" then
        findsignaturefunc = OriginalUtils.PatternScan
        createinterfacefunc = OriginalUtils.CreateInterface
        UnloadFunction = function(fn)
            return Cheat.RegisterCallback('destroy', fn)
        end
    elseif User.Cheat == "legendware" then
        findsignaturefunc = utils.find_signature
        createinterfacefunc = utils.create_interface
        UnloadFunction = function(fn)
            return client.add_callback('unload', fn)
        end
    elseif User.Cheat == "pandora" then
        findsignaturefunc = client.find_sig
        createinterfacefunc = client.create_interface
        UnloadFunction = function(fn)

        end
    elseif User.Cheat == "legion" then
        findsignaturefunc = memory.find_pattern
        createinterfacefunc = memory.create_interface
        UnloadFunction = function(fn)
            return client.add_callback('on_unload', fn)
        end
    elseif User.Cheat == "gamesense" then
        findsignaturefunc = function(moduleName, pattern)
            local gsPattern = ''
            for token in pattern:gmatch('%S+') do
              gsPattern = gsPattern .. (token == '?' and '\xCC' or _G.string.char(tonumber(token, 16)))
            end
            return client.find_signature(moduleName, gsPattern)
        end
        createinterfacefunc = client.create_interface
        UnloadFunction = function(fn)
            return client.set_event_callback('shutdown', fn)
        end
    elseif User.Cheat == "neverlose" then
        findsignaturefunc = utils.opcode_scan
        createinterfacefunc = utils.create_interface
        UnloadFunction = function(fn)

        end
    elseif User.Cheat == "nixware" then
        findsignaturefunc = client.find_pattern
        createinterfacefunc = se.create_interface
        UnloadFunction = function(fn)
            return client.register_callback("unload", fn)
        end
    elseif User.Cheat == "rifk7" then
        findsignaturefunc = function(module_name, pattern)
            local stupid = cast("uint32_t*", engine.signature(module_name, pattern))
            assert(tonumber(stupid) ~= 0)
            return stupid[0]
        end
        createinterfacefunc = function(module_name, interface_name)
            interface_name = string.gsub(interface_name, "%d+", "")
            return general.create_interface(module_name, interface_name)
        end
        UnloadFunction = function(fn)

        end
    elseif User.Cheat == "spirthack" then
        findsignaturefunc = OriginalUtils.PatternScan
        createinterfacefunc = OriginalUtils.CreateInterface
        UnloadFunction = function(fn)

        end
    end

    local FindSignature = function(module_dll, sequence, offset)
        return ffi.cast("unsigned int*", findsignaturefunc(module_dll, sequence)) + (offset or 0)
    end

    if bIsffiCCompatible then
        ffi.cdef[[
            void* GetProcAddress(void* hModule, const char* lpProcName);
            void* GetModuleHandleA(const char* lpModuleName);
        ]]
    else
        Signatures.pGetModuleHandle = FindSignature("engine.dll", " FF 15 ? ? ? ? 85 C0 74 0B") or error('[Signatures] Failed to init pGetModuleHandle')
        Signatures.pGetProcAddress = FindSignature("engine.dll", " FF 15 ? ? ? ? A3 ? ? ? ? EB 05") or error('[Signatures] Failed to init pGetProcAddress')
    end
    
    local GetProcAddress
    local GetModuleHandle
    if User.Cheat == "gamesense" then
        local proxyAddr = FindSignature('engine.dll', '51 C3')
        local fnGetProcAddressAddr = ffi.cast('void*', Signatures.pGetProcAddress)
        local fnGetProcAddressProxy = ffi.cast('uint32_t(__thiscall*)(void*, uint32_t, const char*)', proxyAddr)
        GetProcAddress = function(moduleHandle, functionName)
            return fnGetProcAddressProxy(fnGetProcAddressAddr, moduleHandle, functionName)
        end
        
        local fnGetModuleHandleAddr = ffi.cast('void*', Signatures.pGetModuleHandle)
        local fnGetModuleHandleProxy = ffi.cast('uint32_t(__thiscall*)(void*, const char*)', proxyAddr)
        GetModuleHandle = function(moduleName)
            return fnGetModuleHandleProxy(fnGetModuleHandleAddr, moduleName)
        end
    else
        GetProcAddress = ffi.C and ffi.C.GetProcAddress or ffi.cast("uint32_t(__stdcall*)(uint32_t, const char*)", (ffi.cast("uint32_t**", ffi.cast("uint32_t", Signatures.pGetProcAddress) + 2)[0][0]) )
        GetModuleHandle = ffi.C and ffi.C.GetModuleHandleA or ffi.cast("uint32_t(__stdcall*)(const char*)", (ffi.cast("uint32_t**", ffi.cast("uint32_t", Signatures.pGetModuleHandle) + 2)[0][0]) )    
    end

    local ProcessBind = function(typedef, m_szModuleNameDll, m_szFunctionName)
        return ffi.cast(ffi.typeof(typedef), GetProcAddress(GetModuleHandle(m_szModuleNameDll), m_szFunctionName))
    end

    --[[
    ffi.cdef[[
        typedef void* (*get_interface_fn)();

        typedef struct {
            get_interface_fn get;
            char* name;
            void* next;
        } interface;
    ]]

    local CreateInterface = function(module_dll, interface_name)
        return createinterfacefunc(module_dll, interface_name)
        --[[ -- can be used on some cheats
        if ffi.C then
            return createinterfacefunc(module_dll, interface_name)
        else
            local create_interface_addr = ProcessBind("int", module_dll, "CreateInterface")
            local interface = ffi.cast("interface***", create_interface_addr + ffi.cast("int*", create_interface_addr + 5)[0] + 15)[0][0]

            while interface ~= ffi.NULL do
                if ffi.string(interface.name):match(interface_name .. "%d+") then
                    return interface.get()
                end
            
                interface = ffi.cast("interface*", interface.next)
            end
        end]]
    end

    local VirtualFunction = function(vTable, m_iIndex, typedef)
        return ffi.cast((typedef or "uintptr_t**"), vTable)[0][m_iIndex]
    end

    ffi.cdef[[
        typedef struct {
            uint8_t r;
            uint8_t g;
            uint8_t b;
            uint8_t a;
        } color_struct_t;

        typedef void (*console_color_print)(const color_struct_t&, const char*, ...);
    ]]

    --Signatures.m_fnConsoleColor = ffi.cast("console_color_print", VirtualFunction(CreateInterface("tier0.dll", "?ConColorMsg@@YAXABVColor@@PBDZZ")))
    --Signatures.m_fnConsoleColor = ProcessBind("console_color_print", "tier0.dll", "?ConColorMsg@@YAXABVColor@@PBDZZ")
    --local Print = function (...)
        --[[
        local args = {...}

        local m_arrDrawColor = ffi.new("color_struct_t", {r = 255, g = 255, b = 255, a = 255})
        for i = 1, #args do
            if type(args[i]) == "table" then
                m_arrDrawColor = ffi.new("color_struct_t", {args[i].r or 255, args[i].g or 255, args[i].b or 255, args[i].a or 255})
            elseif args[i] then
                Signatures.m_fnConsoleColor(m_arrDrawColor, args[i])
            end
        end

        Signatures.m_fnConsoleColor(m_arrDrawColor, '\n')]]
    --end
    --print = Print


    Signatures.FindElement = ffi.cast("unsigned long(__thiscall*)(void*, const char*)", FindSignature("client.dll", "55 8B EC 53 8B 5D 08 56 57 8B F9 33 F6 39 77 28"))
    Signatures.CHudChat = Signatures.FindElement(ffi.cast("unsigned long**", ffi.cast("uintptr_t", FindSignature("client.dll", "B9 ? ? ? ? E8 ? ? ? ? 8B 5D 08")) + 1)[0], "CHudChat")
    Signatures.ChatPrint = ffi.cast("void(__cdecl*)(int, int, int, const char*, ...)", ffi.cast("void***", Signatures.CHudChat)[0][27])
    local ChatPrint = function(m_szText)
        Signatures.ChatPrint(Signatures.CHudChat, 0, 0, m_szText)
    end

    local SafeError = function (m_szFunction, m_szError, ret)
        print('[S-API](' .. m_szFunction .. ') ' .. m_szError)
        return ret
    end


    local Error = function (m_szFunction, m_szError, m_iLevel)
        return error('[S-API Critical](' .. m_szFunction .. ') ' .. m_szError, (m_iLevel ~= nil and type(m_iLevel) == "number" and m_iLevel or nil))
    end

    local __thiscall = function(func, this)
        return function(...)
          return func(this, ...)
        end
    end
    
    local VTableBind = function(interface, m_iIndex, typedef)
        if type(m_iIndex) == "string" and type(typedef) == "number" then
            local aux = m_iIndex
            m_iIndex = typedef
            typedef = aux
            aux = nil -- release memory
        end
        if type(m_iIndex) ~= "number" then return Error("VTableBind", "m_iIndex not a number") end
        if type(typedef) ~= "string" then return Error("VTableBind", "m_iIndex not a number") end

        if User.Cheat == "gamesense" then
            local addr = ffi.cast('void***', interface) or error(interface .. ' is nil.')
            return __thiscall(ffi.cast(typedef, addr[0][m_iIndex]), addr)
        end

        local iface = ffi.cast("void***", interface)
        local success, typeof = pcall(ffi.typeof, typedef)
        if not success then error(typeof, 2) end

        local fnptr = ffi.cast(typeof, ffi.cast("void***", iface[0][m_iIndex])) or error(typedef .. ": invalid typecast")
        return function(...)
            return fnptr(tonumber(ffi.cast("void***", iface)), ...)
        end
    end

    local MessageBox
    if bIsffiCCompatible then
        ffi.cdef[[
            int MessageBoxA(void*, const char*, const char*, unsigned);
        ]]
        MessageBox = function(title, text)
            ffi.C.MessageBoxA(0, text, title, 0)
        end
    end

    return {
        FindSignature = FindSignature,
        ProcessBind = ProcessBind,
        CreateInterface = CreateInterface,
        Print = Print,
        ChatPrint = ChatPrint,
        SafeError = SafeError,
        VTableBind = VTableBind,
        MessageBox = MessageBox,
        VirtualFunction = VirtualFunction,
        UnloadFunction = UnloadFunction
    }
end)()
OriginalUtils = nil -- release memory

local Clipboard = (function()
    Signatures.Interface = Utils.CreateInterface('vgui2.dll', 'VGUI_System010')

    Signatures.GetClipboardTextCount = Utils.VTableBind(Signatures.Interface, 7, "int(__thiscall*)(void*)")
    Signatures.SetClipboardText = Utils.VTableBind(Signatures.Interface, 9, "void(__thiscall*)(void*, const char*, int)")
    Signatures.GetClipboardText = Utils.VTableBind(Signatures.Interface, 11, "int(__thiscall*)(void*, int, const char*, int)")

    return {
        Get = function()
            local len = Signatures.GetClipboardTextCount()
            if (len > 0) then
                local char_arr = ffi.typeof("char[?]")(len)
                Signatures.GetClipboardText(0, char_arr, len)
                return ffi.string(char_arr, len - 1)
            end
        end,
        Set = function(m_szText)
            if not m_szText or m_szText == "nil" then return Utils.SafeError("Clipboard.Set", "m_szText is nil") end
            if type(m_szText) ~= "string" then m_szText = tostring(m_szText) end
            Signatures.SetClipboardText(m_szText, string.len(m_szText))
        end
    }
end)()

local Hooks
if User.Cheat == "gamesense" then
    Hooks = { -- placebo cuz gamesense and idk how to bypass that
        Create = function (vt)
            return {
                Hook = function (cast, func, method) end,
                unHook = function (method) end,
                unHookAll = function () end
            }
        end
    }
else
    Hooks = (function()
        local VirtualProtect
    
        if ffi.C then
            ffi.cdef[[
                int VirtualProtect(void* lpAddress, unsigned long dwSize, unsigned long flNewProtect, unsigned long* lpflOldProtect);
            ]]
            
            VirtualProtect = function (lpAddress, dwSize, flNewProtect, lpflOldProtect)
                return ffi.C.VirtualProtect(ffi.cast('void*', lpAddress), dwSize, flNewProtect, lpflOldProtect)
            end
        else
            Signatures.VirtualProtect = Utils.ProcessBind("int(__stdcall*)(void* lpAddress, unsigned long dwSize, unsigned long flNewProtect, unsigned long* lpflOldProtect)", "kernel32.dll", "VirtualProtect")
            VirtualProtect = function(lpAddress, dwSize, flNewProtect, lpflOldProtect)
                return Signatures.VirtualProtect(ffi.cast("void*", lpAddress), dwSize, flNewProtect, lpflOldProtect)
            end
        end
    
        local hooks = {}
        local Create = function(vt)
            local cache = {
                newHook = {},
                orgFunc = {},
                oldProt = ffi.new("unsigned long[1]"),
                virtualTable = ffi.cast("intptr_t**", vt)[0]
            }
    
            cache.newHook.this = cache.virtualTable
            cache.newHook.Hook = function(cast, func, method)
                cache.orgFunc[method] = cache.virtualTable[method]
                VirtualProtect(cache.virtualTable + method, 4, 0x4, cache.oldProt)
    
                cache.virtualTable[method] = ffi.cast("intptr_t", ffi.cast(cast, func))
                VirtualProtect(cache.virtualTable + method, 4, cache.oldProt[0], cache.oldProt)
    
                return ffi.cast(cast, cache.orgFunc[method])
            end
            cache.newHook.unHook = function(method)
                VirtualProtect(cache.virtualTable + method, 4, 0x4, cache.oldProt)
                cache.virtualTable[method] = cache.orgFunc[method]
    
                VirtualProtect(cache.virtualTable + method, 4, cache.oldProt[0], cache.oldProt)
                cache.orgFunc[method] = nil
            end
            cache.newHook.unHookAll = function()
                for method, func in pairs(cache.orgFunc) do
                    cache.newHook.unHookMethod(method)
                end
            end
    
            table.insert(hooks, cache.newHook.unHookAll)
            return cache.newHook
        end
    
        return {Create = Create}
    end)()
end

local ClientState = (function()
    ffi.cdef[[
        typedef struct
        {
            float m_ClockOffsets[ 16 ];
            int m_iCurClockOffset;
	        int m_nServerTick;
	        int	m_nClientTick;
        } CClockDriftMgr;

        typedef struct
        {
            /*
            bool SendNetMsg( INetMessage* msg , bool rel = false , bool voice = false ) {
                return util::get_virtual_function< bool( __thiscall* )( void* , void* , bool , bool ) >( this , 40 )( this , msg , rel , voice );
            }
            
            int SendDatagram( bf_write* data = NULL )
            {
                return util::get_virtual_function< int( __thiscall* )( void* , bf_write* ) >( this , 46 )( this , data );
            }
            */
            uint8_t pad_0x14[ 0x14 ];
            bool m_bProcessingMessages;		// 0x0014
            bool m_bShouldDelete;			// 0x0015
            uint8_t pad_0x2[ 0x2 ];
            int m_nOutSequenceNr;			// 0x0018 last send outgoing sequence number
            int m_nInSequenceNr;			// 0x001C last received incoming sequnec number
            int m_nOutSequenceNrAck;		// 0x0020 last received acknowledge outgoing sequnce number
            int m_nOutReliableState;		// 0x0024 state of outgoing reliable data (0/1) flip flop used for loss detection
            int m_nInReliableState;			// 0x0028 state of incoming reliable data
            int m_nChokedPackets;			// 0x002C number of choked packets
            uint8_t pad_0x414[ 0x414 ];					// 0x0030
        } INetChannel;

        typedef struct
        {
            char pad_0000 [ 156 ];
	        INetChannel* m_NetChannel;
	        int m_nChallengeNr;
	        char pad_00A4 [ 100 ];
	        int m_nSignonState;
	        int signon_pads [ 2 ];
	        float m_flNextCmdTime;
	        int m_nServerCount;
	        int m_nCurrentSequence;
	        int musor_pads [ 2 ];
	        CClockDriftMgr m_ClockDriftMgr;
	        int m_nDeltaTick;
	        bool m_bPaused;
	        char paused_align [ 3 ];
	        int m_nViewEntity;
	        int m_nPlayerSlot;
	        int bruh;
	        char m_szLevelName [ 260 ];
	        char m_szLevelNameShort [ 80 ];
	        char m_szGroupName [ 80 ];
	        char pad_032 [ 92 ];
	        int m_iMaxClients;
	        char pad_0314 [ 18828 ];
	        float m_iLastServerTickTime;
	        bool m_bInSimulation;
	        char pad_4C9D [ 3 ];
	        int m_iOldTickcount;
	        float m_flTickRemainder;
	        float m_flFrameTime;
	        int m_iLastOutGoingConnect;
	        int m_iChockedCommands;
	        int m_iLastCommandAck;
	        int m_iLastServerTick;
	        int m_iCommandAck;
	        int m_nSoundSequence;
	        char pad_4CCD [ 76 ];
	        vec3_t m_vecViewAngles;
	        int pads [ 54 ];
	        void* m_nEvents;
        } CClientState;
    ]]

    Signatures.CClientStateAddress = Utils.FindSignature("engine.dll", "A1 ? ? ? ? 33 D2 6A 00 6A 00 33 C9 89 B0", 1)

    return ffi.cast("CClientState***", Signatures.CClientStateAddress)[0][0][0]
end)()

local NetGraph = (function ()
    ffi.cdef[[
        typedef struct
        {
            void* vtable;
            unsigned char gap4 [ 72 ];
            unsigned long dword4C;
            unsigned char gap50 [ 20 ];
            unsigned short word64;
            unsigned char gap66 [ 42 ];
            unsigned long dword90;
            unsigned char gap94 [ 16 ];
            unsigned long dwordA4;
            unsigned char gapA8 [ 244 ];
            unsigned short word19C;
            unsigned char gap19E [ 3 ];
            unsigned char byte1A1;
            unsigned short word1A2;
            unsigned char byte1A4;
            unsigned char gap1A5 [ 19 ];
            char char1B8;
            unsigned char gap1B9 [ 77823 ];
            float m_FrameRate;
            float m_AvgLatency;
            float m_AvgPacketLoss;
            float m_AvgPacketChoke;
            int m_IncomingSequence;
            int m_OutgoingSequence;
            unsigned long m_UpdateWindowSize;
            float m_IncomingData;
            float m_OutgoingData;
            float m_AvgPacketIn;
            float m_AvgPacketOut;
            unsigned char gap131E4 [ 16 ];
            int dword131F4;
            unsigned long dword131F8;
            unsigned long dword131FC;
            unsigned long dword13200;
            unsigned long dword13204;
            unsigned long m_hFontProportional;
            unsigned long m_hFont;
            unsigned long dword13210;
            void* cl_updaterate;
            void* cl_cmdrate;
            unsigned long dword1321C;
            unsigned long dword13220;
            unsigned long dword13224;
            unsigned long dword13228;
            unsigned long dword1322C;
            unsigned long dword13230;
            unsigned long dword13234;
            unsigned long m_nNetGraphHeight;
            unsigned long dword1323C;
            unsigned long dword13240;
            unsigned long dword13244;
            unsigned long dword13248;
        } CNetGraphPanel;
    ]]
    local pNetGraphPanelAddr = Utils.FindSignature("client.dll", "89 1D ? ? ? ? 8B C3 5B 8B E5 5D C2 04 00", 2)

    return ffi.cast("CNetGraphPanel***", pNetGraphPanelAddr)[0][0]
end)()

local Engine = (function ()
    Signatures.VEngineClient014 = Utils.CreateInterface("engine.dll", "VEngineClient014")

    Signatures.InGameVFunc = Utils.VTableBind(Signatures.VEngineClient014, 26, "bool( __thiscall* )( void* )")
    local InGame = function ()
        return Signatures.InGameVFunc()
    end

    Signatures.IsConnectedVFunc = Utils.VTableBind(Signatures.VEngineClient014, 27, "bool( __thiscall* )( void* )")
    local IsConnected = function ()
        return Signatures.IsConnectedVFunc()
    end

    Signatures.GetScreenSizeVFunc = Utils.VTableBind(Signatures.VEngineClient014, 5, "void(__thiscall*)(void*, int&, int&)")
    local GetScreenSize = function ()
        local w_ptr = ffi.typeof("int[1]")()
        local h_ptr = ffi.typeof("int[1]")()
        
        Signatures.GetScreenSizeVFunc(w_ptr, h_ptr)

        return vec2_t(tonumber(w_ptr[0]), tonumber(h_ptr[0]))
    end

    ffi.cdef[[
        typedef struct {
            uint64_t      m_uDataMap;
            union {
                int64_t   m_iSteamID64;
                struct {
                    int   m_iSteamIDLow;
                    int   m_iSteamIDHigh;
                };
            };
            char          m_szName [ 128 ];
            int           m_iUserID;
            char          m_szGUID [ 33 ];
            uint32_t      m_uFriendID;
            char          m_szFriendName [ 128 ];
            bool          m_bIsFakePlayer;
            bool          m_bIsHLTV;
            uint32_t      m_uCustomFiles [ 4 ];
            uint8_t       m_uFilesDownloaded;
        } player_info_t;
    ]]

    Signatures.GetPlayerInfoVFunc = Utils.VTableBind(Signatures.VEngineClient014, 8, "bool( __thiscall* )( void* , int , player_info_t* )")
    local GetPlayerInfo = function(m_iIndex)
        if not InGame() then return end

        local m_player_info = ffi.typeof("player_info_t")()

        Signatures.GetPlayerInfoVFunc(ffi.cast("int", m_iIndex), m_player_info)

        return m_player_info
    end

    Signatures.GetPlayerByUserId = Utils.VTableBind(Signatures.VEngineClient014, 9, "int( __thiscall* )( void* , int )")
    local GetPlayerByUserId = function (m_iUserID)
        if not InGame() then return end

        return Signatures.GetPlayerByUserId(ffi.cast("int", m_iUserID))
    end

    Signatures.GetViewAngles = Utils.VTableBind(Signatures.VEngineClient014, 18, "void( __thiscall* )( void* , vec3_t& )")
    local GetViewAngles = function ()
        if not InGame() then return end

        local angle = ffi.typeof("vec3_t[1]")()

        Signatures.GetViewAngles(angle)

        return vec3_t(angle[0])
    end

    Signatures.SetViewAngles = Utils.VTableBind(Signatures.VEngineClient014, 19, "void( __thiscall* )( void* , vec3_t& )")
    local SetViewAngles = function (m_vecAngle)
        if not InGame() then return end

        local angle = ffi.typeof("vec3_t[1]")()
        angle.x = m_vecAngle.x
        angle.y = m_vecAngle.y
        angle.z = m_vecAngle.z

        Signatures.SetViewAngles(angle)
    end

    Signatures.ExecuteCMD = Utils.VTableBind(Signatures.VEngineClient014, 108, "void( __thiscall* )( void* , const char* )")
    local Execute = function(cmd)
        return Signatures.ExecuteCMD(ffi.cast("const char*", cmd))
    end

    Signatures.IsPausedVFunc = Utils.VTableBind(Signatures.VEngineClient014, 90, "bool( __thiscall* )( void* )")
    local IsPaused = function ()
        return Signatures.IsPausedVFunc()
    end

    Signatures.IsHLTVVFunc = Utils.VTableBind(Signatures.VEngineClient014, 90, "bool( __thiscall* )( void* )")
    local IsHLTV = function ()
        return Signatures.IsHLTVVFunc()
    end

    ffi.cdef[[
        typedef struct {
        	uint64_t m_uiCaseID;
        	uint32_t m_uiHeaderPrefixLength;
        	uint32_t m_uiLockFirstPersonAccountID;
        	bool m_bAnonymousPlayerIdentity;
        	uint32_t m_numRoundSkip;
        	uint32_t m_numRoundStop;
        	bool m_bSkipWarmup;
        	bool m_bPlayingLiveRemoteBroadcast;
        	uint64_t m_uiLiveMatchID;
        } CDemoPlaybackParameters_t;
    ]]
    
    Signatures.GetDemoPlaybackParameters = Utils.VTableBind(Signatures.VEngineClient014, 218, "CDemoPlaybackParameters_t* ( __thiscall* )( void* )")
    local GetDemoPlaybackParameters = function()
        return Signatures.GetDemoPlaybackParameters()
    end

    Signatures.GetScreenAspectRatio = Utils.VTableBind(Signatures.VEngineClient014, 101, "float( __thiscall* )( void* , int , int )")
    local GetScreenAspectRatio = function (viewportWidth, viewportHeight)
        return Signatures.GetScreenAspectRatio(viewportWidth, viewportHeight)
    end

    return {
        GetScreenSize = GetScreenSize,
        GetPlayerInfo = GetPlayerInfo,
        GetPlayerByUserId = GetPlayerByUserId,
        Execute = Execute,
        IsConnected = IsConnected,
        InGame = InGame,
        IsPaused = IsPaused,
        IsHLTV = IsHLTV,
        GetViewAngles = GetViewAngles,
        SetViewAngles = SetViewAngles,
        GetDemoPlaybackParameters = GetDemoPlaybackParameters,
        GetScreenAspectRatio = GetScreenAspectRatio
    }
end)()

local NetChannel = (function ()
    return ClientState.m_NetChannel ~= ffi.NULL and ClientState.m_NetChannel or {
        m_bProcessingMessages = false,
        m_bShouldDelete = false,
        m_nOutSequenceNr = 0,
        m_nInSequenceNr = 0,
        m_nOutSequenceNrAck = 0,
        m_nOutReliableState = 0,
        m_nInReliableState = 0,
        m_nChokedPackets = 0,
    }

    --[[
    local SendDatagram = function(data)
        --return Utils.VirtualFunction("int(__thiscall*)(void*, void*)", 46)(data)
    end
    
    local SendNetMsg = function (msg, rel, voice)
        
    end]]
end)()

--[[
local NetChannelInfo = (function ()
    local Flow = {
        OUTGOING = 0,
        INCOMING = 1,
        MAX = 2
    }
    Signatures.GetNetChannelInfo = Utils.VTableBind(Signatures.VEngineClient014, 218, "void* ( __thiscall* )( void* )")

    local Valid = function ()
        return Engine.InGame() and Signatures.GetNetChannelInfo() ~= ffi.NULL
    end

    Signatures.GetSequenceNr = Utils.VTableBind(Signatures.GetNetChannelInfo(), 17, "int(__thiscall*)(void*, int)")
    local GetSequenceNr = function ()
        if not Valid() then return 0 end
        return Signatures.GetSequenceNr(1)
    end

    Signatures.GetLoopBack = Utils.VTableBind(Signatures.GetNetChannelInfo(), 6, "bool(__thiscall*)(void*)")
    local GetLoopBack = function ()
        if not Valid() then return false end
        return Signatures.GetLoopBack()
    end

    Signatures.TimingOut = Utils.VTableBind(Signatures.GetNetChannelInfo(), 7, "bool(__thiscall*)(void*)")
    local TimingOut = function ()
        if not Valid() then return false end
        return Signatures.TimingOut()
    end

    Signatures.GetCurrentLatency = Utils.VTableBind(Signatures.GetNetChannelInfo(), 9, "float(__thiscall*)(void*, int)")
    local GetCurrentLatency = function (m_iFlow)
        if not Valid() then return 0 end
        return Signatures.GetCurrentLatency(m_iFlow)
    end

    Signatures.GetAverageLatency = Utils.VTableBind(Signatures.GetNetChannelInfo(), 10, "float(__thiscall*)(void*, int)")
    local GetAverageLatency = function (m_iFlow)
        if not Valid() then return 0 end
        return Signatures.GetAverageLatency(m_iFlow)
    end

    Signatures.GetPacketLoss = Utils.VTableBind(Signatures.GetNetChannelInfo(), 11, "float(__thiscall*)(void*, int)")
    local GetPacketLoss = function ()
        if not Valid() then return 0 end
        return Signatures.GetPacketLoss(1)
    end
    
    Signatures.GetChoke = Utils.VTableBind(Signatures.GetNetChannelInfo(), 12, "float(__thiscall*)(void*, int)")
    local GetChoke = function ()
        if not Valid() then return 0 end
        return Signatures.GetChoke(1)
    end
    
    Signatures.GetReceivedBytes = Utils.VTableBind(Signatures.GetNetChannelInfo(), 13, "float(__thiscall*)(void*, int)")
    local GetReceivedBytes = function ()
        if not Valid() then return 0 end
        return Signatures.GetReceivedBytes(1)
    end
    
    Signatures.GetSentBytes = Utils.VTableBind(Signatures.GetNetChannelInfo(), 14, "float(__thiscall*)(void*, int)")
    local GetSentBytes = function ()
        if not Valid() then return 0 end
        return Signatures.GetSentBytes(1)
    end
    
    Signatures.GetValidPacket = Utils.VTableBind(Signatures.GetNetChannelInfo(), 18, "bool(__thiscall*)(void*, int, int)")
    local GetValidPacket = function ()
        if not Valid() then return false end
        return Signatures.GetValidPacket(1, GetSequenceNr() - 1)
    end
    
    return {
        Flow = Flow,
        Valid = Valid,
        GetSequenceNr = GetSequenceNr,
        GetLoopBack = GetLoopBack,
        TimingOut = TimingOut,
        GetCurrentLatency = GetCurrentLatency,
        GetAverageLatency = GetAverageLatency,
        GetPacketLoss = GetPacketLoss,
        GetChoke = GetChoke,
        GetReceivedBytes = GetReceivedBytes,
        GetSentBytes = GetSentBytes,
        GetValidPacket = GetValidPacket
    }
end)()]]

--[[
local NetChannelInfo = (function ()
    Signatures.VEngineClient014 = Utils.CreateInterface("engine.dll", "VEngineClient014")
    Signatures.EngineClientVCast = ffi.cast("void***", Signatures.VEngineClient014)
    Signatures.GetNetChannelInfo = ffi.cast("void*(__thiscall*)(void*)", Signatures.EngineClientVCast[0][78])
    Signatures.INetChannelInfoPtr = ffi.cast("void***", Signatures.GetNetChannelInfo(Signatures.EngineClientVCast))
    Signatures.INetChannelInfo = Signatures.INetChannelInfoPtr[0]

    local Flow = {
        OUTGOING = 0,
        INCOMING = 1,
        MAX = 2
    }

    local Valid = function ()
        return Engine.m_bInGame() and Signatures.EngineClientVCast[0][78] ~= ffi.NULL
    end

    local GetSequenceNr = function ()
        return ffi.cast("int(__thiscall*)(void*, int)", Signatures.INetChannelInfo[17])(Signatures.INetChannelInfoPtr, 1)
    end

    local GetLoopBack = function ()
        return ffi.cast("bool(__thiscall*)(void*)", Signatures.INetChannelInfo[6])(Signatures.INetChannelInfoPtr)
    end

    local TimingOut = function ()
        return ffi.cast("bool(__thiscall*)(void*)", Signatures.INetChannelInfo[7])(Signatures.INetChannelInfoPtr)
    end

    local GetCurrentLatency = function (m_iFlow)
        return ffi.cast("float(__thiscall*)(void*, int)", Signatures.INetChannelInfo[9])(Signatures.INetChannelInfoPtr, m_iFlow)
    end

    local GetAverageLatency = function (m_iFlow)
        return ffi.cast("float(__thiscall*)(void*, int)", Signatures.INetChannelInfo[10])(Signatures.INetChannelInfoPtr, m_iFlow)
    end

    local GetPacketLoss = function ()
        return ffi.cast("float(__thiscall*)(void*, int)", Signatures.INetChannelInfo[11])(Signatures.INetChannelInfoPtr, 1)
    end
    
    local GetChoke = function ()
        return ffi.cast("float(__thiscall*)(void*, int)", Signatures.INetChannelInfo[12])(Signatures.INetChannelInfoPtr, 1)
    end
    
    local GetReceivedBytes = function ()
        return ffi.cast("float(__thiscall*)(void*, int)", Signatures.INetChannelInfo[13])(Signatures.INetChannelInfoPtr, 1)
    end
    
    local GetSentBytes = function ()
        return ffi.cast("float(__thiscall*)(void*, int)", Signatures.INetChannelInfo[12])(Signatures.INetChannelInfoPtr, 1)
    end
    
    local GetValidPacket = function ()
        return ffi.cast("bool(__thiscall*)(void*, int, int)", Signatures.INetChannelInfo[18])(Signatures.INetChannelInfoPtr, 1, GetSequenceNr() -1)
    end

    return {
        Flow = Flow,
        Valid = Valid,
        GetSequenceNr = GetSequenceNr,
        GetLoopBack = GetLoopBack,
        TimingOut = TimingOut,
        GetCurrentLatency = GetCurrentLatency,
        GetAverageLatency = GetAverageLatency,
        GetPacketLoss = GetPacketLoss,
        GetChoke = GetChoke,
        GetReceivedBytes = GetReceivedBytes,
        GetSentBytes = GetSentBytes,
        GetValidPacket = GetValidPacket
    }
end)()]]

local Entity = (function ()
    ffi.cdef[[
        typedef struct
        {
            char        pad0[0x60]; // 0x00
            void*       m_nEntity; // 0x60
            void*       m_nActiveWeapon; // 0x64
            void*       m_nLastActiveWeapon; // 0x68
            float       m_flLastUpdateTime; // 0x6C
            int         m_iLastUpdateFrame; // 0x70
            float       m_flLastUpdateIncrement; // 0x74
            float       m_flEyeYaw; // 0x78
            float       m_flEyePitch; // 0x7C
            float       m_flGoalFeetYaw; // 0x80
            float       m_flLastFeetYaw; // 0x84
            float       m_flMoveYaw; // 0x88
            float       m_flLastMoveYaw; // 0x8C // changes when moving/jumping/hitting ground
            float       m_flLeanAmount; // 0x90
            char        pad1[0x4]; // 0x94
            float       m_flFeetCycle; // 0x98 0 to 1
            float       m_flMoveWeight; // 0x9C 0 to 1
            float       m_flMoveWeightSmoothed; // 0xA0
            float       m_flDuckAmount; // 0xA4
            float       m_flHitGroundCycle; // 0xA8
            float       m_flRecrouchWeight; // 0xAC
            vec3_t      m_vecOrigin; // 0xB0
            vec3_t      m_vecLastOrigin;// 0xBC
            vec3_t      m_vecVelocity; // 0xC8
            vec3_t      m_vecVelocityNormalized; // 0xD4
            vec3_t      m_vecVelocityNormalizedNonZero; // 0xE0
            float       m_flVelocityLenght2D; // 0xEC
            float       m_flJumpFallVelocity; // 0xF0
            float       m_flSpeedNormalized; // 0xF4 // clamped velocity from 0 to 1
            float       m_flRunningSpeed; // 0xF8
            float       m_flDuckingSpeed; // 0xFC
            float       m_flDurationMoving; // 0x100
            float       m_flDurationStill; // 0x104
            bool        m_bOnGround; // 0x108
            bool        m_bHitGroundAnimation; // 0x109
            char        pad2[0x2]; // 0x10A
            float       m_flNextLowerBodyYawUpdateTime; // 0x10C
            float       m_flDurationInAir; // 0x110
            float       m_flLeftGroundHeight; // 0x114
            float       m_flHitGroundWeight; // 0x118 // from 0 to 1, is 1 when standing
            float       m_flWalkToRunTransition; // 0x11C // from 0 to 1, doesnt change when walking or crouching, only running
            char        pad3[0x4]; // 0x120
            float       m_flAffectedFraction; // 0x124 // affected while jumping and running, or when just jumping, 0 to 1
            char        pad4[0x208]; // 0x128
            char        pad_because_yes[0x4]; // 0x330
            float       m_flMinBodyYaw; // 0x330 + 0x4
            float       m_flMaxBodyYaw; // 0x334 + 0x4
            float       m_flMinPitch; //0x338 + 0x4
            float       m_flMaxPitch; // 0x33C + 0x4
            int         m_iAnimsetVersion; // 0x340 + 0x4
        } CCSGOPlayerAnimationState_t;

        typedef struct
        {
            float   m_flAnimationTime;		
            float   m_flFadeOutTime;	
            int     m_iFlags;			
            int     m_iActivity;			
            int     m_iPriority;			
            int     m_iOrder;			
            int     m_iSequence;			
            float   m_flPrevCycle;		
            float   m_flWeight;			
            float   m_flWeightDeltaRate;
            float   m_flPlaybackRate;	
            float   m_flCycle;			
            void*   m_nOwner;			
            int     m_iBits;				
        } C_AnimationLayer;
    ]]
    local CEntityListData = {}
    local CFoundEntity = {}

    -- Declared in Engine --Signatures.VEngineClient014 = Utils.CreateInterface( "engine.dll", "VEngineClient014" )
    Signatures.VClientEntityList003 = Utils.CreateInterface( "client.dll", "VClientEntityList003" )

    Signatures.VEngineClient014VFTable = ffi.cast( "void***", Signatures.VEngineClient014 )
    Signatures.VClientEntityList003VFTable = ffi.cast( "void***", Signatures.VClientEntityList003 )

    Signatures.GetLocalPlayer = ffi.cast( "int( __thiscall* )( void* )", Utils.VirtualFunction( Signatures.VEngineClient014, 12 ) )
    Signatures.GetClientEntity = ffi.cast( "void*( __thiscall* )( void*, int )", Utils.VirtualFunction( Signatures.VClientEntityList003, 3 ) )
    Signatures.GetClientHandle = ffi.cast( "void*( __thiscall* )( void*, int )", Utils.VirtualFunction( Signatures.VClientEntityList003, 4 ) )

    CEntityListData.Registered = {
        m_iIndex = -1,
        m_szType = 'Entity',
        m_hAddress = nil
    }

    function CEntityListData.Registered:new(o)
        o = o or {}
        setmetatable(o, self)
        self.__index = self
        return o
    end

    local GetLocalPlayer = function ()
        return Signatures.GetLocalPlayer(Signatures.VEngineClient014VFTable)
    end

    local Get = function(m_iIndex)
        if m_iIndex < 1 then return end
        if not Engine.m_bInGame() then return end
        local m_hAddress = Signatures.GetClientEntity(Signatures.EntityListVTable, m_iIndex)
        if not m_hAddress or m_hAddress == ffi.NULL then return end
        --if CFoundEntity[m_hAddress] then return CFoundEntity[m_hAddress] end
        return CEntityListData.Registered:new({
            m_iIndex = m_iIndex,
            m_szType = 'Entity',
            m_hAddress = m_hAddress
        })
    end

    local GetHandle = function(m_iHandle)
        if m_iHandle < 1 then return end
        if not Engine.m_bInGame() then return end
        local m_hAddress = Signatures.GetClientEntity(Signatures.EntityListVTable, m_iHandle)
        if not m_hAddress or m_hAddress == ffi.NULL then return end
        return CEntityListData.Registered:new({
            m_iIndex = m_iHandle,
            m_szType = 'Handle',
            m_hAddress = m_hAddress
        })
    end

    local Find = function(m_hAddress)
        if CFoundEntity[m_hAddress] then return CFoundEntity[m_hAddress] end

        for i = 1, ClientState.m_iMaxClients do
            if not Get(i) then goto continue end

            if Get(i).m_hAddress == m_hAddress then
                CFoundEntity[m_hAddress] = Get(i)
            end

            ::continue::
        end

        return CFoundEntity[m_hAddress]
    end

    function CEntityListData.Registered:GetProp(typedef, m_hOffset, m_hPointer)
        return ffi.cast((typedef .. "*"), ffi.cast('unsigned int', self.m_hAddress) + m_hOffset)[(m_hPointer or 0)]
    end

    function CEntityListData.Registered:m_iHealth()
        return ffi.cast('int*', ffi.cast('unsigned int', self.m_hAddress) + 0x100)[0]
    end

    function CEntityListData.Registered:m_iTeamNum()
        return ffi.cast('int*', ffi.cast('unsigned int', self.m_hAddress) + 0xF4)[0]
    end

    function CEntityListData.Registered:m_nAnimationLayers()
        return ffi.cast("C_AnimationLayer**", ffi.cast("uintptr_t", self.m_hAddress) + 0x2990)[0]
    end

    local m_PlayerAnimState = ffi.cast("uintptr_t*", Utils.FindSignature("client.dll", "8B 8E ? ? ? ? F3 0F 10 48 04 E8 ? ? ? ? E9") + 2)[0]
    function CEntityListData.Registered:m_nAnimationState()
        return ffi.cast("CCSGOPlayerAnimationState_t**", ffi.cast("uintptr_t", self.m_hAddress) + m_PlayerAnimState)[0]
    end

    function CEntityListData.Registered:m_nPoseParameters()
        return ffi.cast("float*", ffi.cast("uintptr_t", self.m_hAddress) + 10104)
    end

    function CEntityListData.Registered:m_bDormant()
        return ffi.cast("bool*", ffi.cast("uintptr_t", self.m_hAddress) + 0xED)[0]
    end

    function CEntityListData.Registered:m_fFlags()
        return ffi.cast("float*", ffi.cast("uintptr_t", self.m_hAddress) + 0x104)[0]
    end

    function CEntityListData.Registered:m_vecVelocity()
        return ffi.cast('vec3_t*', ffi.cast('unsigned int', self.m_hAddress) + 0x114)[0]
    end

    function CEntityListData.Registered:m_flVelocity()
        return vec3_t(self:m_vecVelocity()):Length2d()
    end

    function CEntityListData.Registered:m_flSimulationTime()
        return ffi.cast('float*', ffi.cast('uintptr_t', self.m_hAddress) + 0x268)[0]
    end

    function CEntityListData.Registered:m_flOldSimulationTime()
        return ffi.cast('float*', ffi.cast('uintptr_t', self.m_hAddress) + 0x268 + ffi.sizeof('float'))[0]
    end

    function CEntityListData.Registered:m_flLowerBodyYawTarget()
        return ffi.cast('float*', ffi.cast('uintptr_t', self.m_hAddress) + 0x9ADC)[0]
    end

    return {
        GetLocalPlayer = GetLocalPlayer,
        Get = Get,
        GetHandle = GetHandle,
        Find = Find
    }
end)()

local Input = (function ()
    Signatures.GetScrollInterface = Utils.CreateInterface('inputsystem.dll', 'InputSystemVersion001') or error('[S-API](Signatures.GetScrollInterface) Invalid or unable to found scroll interface')
    Signatures.GetScrollAddress = ffi.cast(ffi.typeof('void***'), Signatures.GetScrollInterface)
    Signatures.GetScrollVMT = Utils.VTableBind(Signatures.GetScrollAddress, 21, 'const struct {int m_nType, m_nTick, m_nData, m_nData2, m_nData3;}*(__thiscall*)(void*)')
    local PrevScrollTick = 0

    local GetScroll = function ()
        local data = Signatures.GetScrollVMT()
        if not data then
            return 0
        end
        if data.m_nTick ~= PrevScrollTick then
            PrevScrollTick = data.m_nTick;
            return ((data.m_nData == 112 and 1 or (data.m_nData == 113 and -1 or 0)))
        end
        return 0
    end

    ffi.cdef[[
        typedef unsigned long HWND;
        typedef int BOOL;

        typedef struct{
            long x, y;
        }POINT, *LPPOINT;

        short GetAsyncKeyState(int vKey);
        BOOL GetCursorPos(LPPOINT);
        BOOL IsChild(HWND hWndParent, HWND hWnd);
        BOOL ScreenToClient(HWND hWnd, LPPOINT lpPoint);
    ]]

    local IsKeyPressed = function(vKey)
        return ffi.C.GetAsyncKeyState((vKey or 1)) < 0
    end

    local vKeyToggled = {}
    local IsKeyToggled = function(vKey)
        if not vKeyToggled[vKey] then vKeyToggled[vKey] = {false, false} end
        if not vKeyToggled[vKey][1] then
            vKeyToggled[vKey][2] = (bit.band(ffi.C.GetAsyncKeyState((vKey or 1)), 1) ~= 0)
        end
        vKeyToggled[vKey][1] = IsKeyPressed(vKey)
        return vKeyToggled[vKey][2]
    end

    local GetCursorPosition = function ()
        local csgo_window = ffi.C.FindWindowA("Valve001", nil)
        local hActiveWindow = ffi.C.GetForegroundWindow()
        if hActiveWindow == 0 then
            return vec2_t()
        end

        if hActiveWindow ~= csgo_window and not ffi.C.IsChild(hActiveWindow, csgo_window) then
            return vec2_t()
        end

        local ppoint = ffi.new("POINT[1]")
        if ffi.C.GetCursorPos(ppoint) == 0 then
            return vec2_t()
        end

        if not ffi.C.ScreenToClient(csgo_window, ppoint) then
            return vec2_t()
        end

        return vec2_t(ppoint[0].x, ppoint[0].y)
    end

    -- Vectors Extended
    function vector_data_t:Hovered(dim)
        local mouse_pos = GetCursorPosition()
        return ((mouse_pos.x >= self.x) and (mouse_pos.x <= self.x + dim.x)) and
                   ((mouse_pos.y >= self.y) and (mouse_pos.y <= self.y + dim.y))
    end

    local vector_dragging_data = {}
    function vector_data_t:Drag(vec2d, m_szName, m_bShouldBlockDragging)
        if not vector_dragging_data[m_szName] then
            vector_dragging_data[m_szName] = {
                x = self.x,
                y = self.y,
                clicked = false,
                drag = false,
                memory_x = 0,
                memory_y = 0
            }
        end
        vector_dragging_data[m_szName].w = vec2d.x
        vector_dragging_data[m_szName].h = vec2d.y
    
        local mouse_pos = GetCursorPosition()
        local is_hovering_area = vec2_t(vector_dragging_data[m_szName].x, vector_dragging_data[m_szName].y):Hovered(vec2_t(vector_dragging_data[m_szName].w, vector_dragging_data[m_szName].h))
        local key = 1
    
        if IsKeyPressed(key) and not m_bShouldBlockDragging then
            if not vector_dragging_data[m_szName].clicked then
                if is_hovering_area then
                    vector_dragging_data[m_szName].drag = true
                    vector_dragging_data[m_szName].memory_x = vector_dragging_data[m_szName].x - mouse_pos.x
                    vector_dragging_data[m_szName].memory_y = vector_dragging_data[m_szName].y - mouse_pos.y
                end
    
                vector_dragging_data[m_szName].clicked = true
            end
        else
            vector_dragging_data[m_szName].clicked = false
            vector_dragging_data[m_szName].drag = false
        end
    
        if vector_dragging_data[m_szName].drag then
            vector_dragging_data[m_szName].x = mouse_pos.x + vector_dragging_data[m_szName].memory_x
            vector_dragging_data[m_szName].y = mouse_pos.y + vector_dragging_data[m_szName].memory_y
        end
    
        self.x = vector_dragging_data[m_szName].x
        self.y = vector_dragging_data[m_szName].y
    
        return self
    end

    return {
        GetScroll = GetScroll,
        IsKeyPressed = IsKeyPressed,
        IsKeyToggled = IsKeyToggled,
        GetCursorPosition = GetCursorPosition
    }
end)()

local Callbacks = (function ()
    local Draw = {}
    local CreateMove = {}
    local FrameStage = {}
    local Animations = {}

    (function () -- Surface
        local oPaintTraverse

        Signatures.VGUI_Panel009 = Utils.CreateInterface("vgui2.dll", "VGUI_Panel009")
        Signatures.VGUI_Panel009VTable = ffi.cast("void***", Signatures.VGUI_Panel009)[0]
        Signatures.GetPanelName = ffi.cast("const char*(__thiscall*)(void*, uint32_t)", Signatures.VGUI_Panel009VTable[36])

        function hkPaintTraverse(panel, vguiPanel, forceRepaint, allowForce)
            if bUnloaded then
                return oPaintTraverse(panel, vguiPanel, forceRepaint, allowForce)
            end
            local panelName = ffi.string(Signatures.GetPanelName(panel, vguiPanel))
            if (panelName == "FocusOverlayPanel") then
                pcall(function()
                    for _, func in pairs(Draw) do
                        func()
                    end
                end)
            end
            oPaintTraverse(panel, vguiPanel, forceRepaint, allowForce)
        end

        oPaintTraverse = Hooks.Create(Signatures.VGUI_Panel009).Hook("void(__thiscall*)(void*, unsigned int, bool, bool)", hkPaintTraverse, 41)

        Utils.UnloadFunction(function ()
            oPaintTraverse.unHookAll()
        end)
    end)()

    local UpdateClientSideAnimationHook = function () -- UpdateClientSideAnimations
        local oUpdateClientSideAnimation

        function hkUpdateClientSideAnimation(m_nEntity, edx)
            if bUnloaded then
                return oUpdateClientSideAnimation(m_nEntity, edx)
            end

            function UpdateAnimations()
                return oUpdateClientSideAnimation(m_nEntity, edx)
            end

            pcall(function ()
                for _, func in pairs(Animations) do
                    func(Entity.Find(m_nEntity), UpdateAnimations)
                end
            end)

            return oUpdateClientSideAnimation(m_nEntity, edx)
        end

        function nHookFunction()
            if oUpdateClientSideAnimation then return end
            local m_local_index = Entity.GetLocalPlayer()
            if m_local_index < 1 then return end

            local m_local = Entity.Get(m_local_index)
            if not m_local then return end
            if m_local:m_iHealth() < 1 then return end

            oUpdateClientSideAnimation = Hooks.Create(ffi.cast('void*', m_local.m_hAddress)).Hook("void(__fastcall*)(void*, void*)", hkUpdateClientSideAnimation, 224)
        end

        Utils.UnloadFunction(function ()
            oUpdateClientSideAnimation.unHookAll()
        end)
        
        return nHookFunction
    end

    (function () -- CreateMove
        ffi.cdef[[
            typedef struct {
                int command_number;
                int tick_count;
                vec3_t viewangles;
                vec3_t aimdirection;
                float forwardmove;
                float sidemove;
                float upmove;
                int buttons;
                char impulse;
                int weaponselect;
                int weaponsubtype;
                int random_seed;
                short mousedx;
                short mousedy;
                bool hasbeenpredicted;
                vec3_t headangles;
                vec3_t headoffset;
            } CUserCmd;
        ]]
    
        local oCreateMove

        Signatures.IClientModeH = ffi.cast("void***", ffi.cast("unsigned int", ffi.cast("void***", Utils.CreateInterface("client.dll", "VClient018"))[0][10]) + 0x5)[0][0]
        
        function hkCreateMove(m_flTime, CUserCmd)
            if bUnloaded then
                return oCreateMove(m_flTime, CUserCmd)
            end
            local ret = oCreateMove(m_flTime, CUserCmd)
            pcall(function ()
                UpdateClientSideAnimationHook()
                for _, func in pairs(CreateMove) do
                    func(CUserCmd)
                end
            end)
            return false
        end

        oCreateMove = Hooks.Create(Signatures.IClientModeH).Hook("bool(__stdcall*)(float, CUserCmd*)", hkCreateMove, 24)

        Utils.UnloadFunction(function ()
            oCreateMove.unHookAll()
        end)
    end)()

    -- doesn t load if i just load this without the variable in front
    local fsn = (function () -- FrameStage
        local oFrameStageNotify

        Signatures.VClient018 = ffi.cast("void*", Utils.CreateInterface("client.dll", "VClient018"))

        function hkFrameStageNotify(m_iStage)
            if bUnloaded then
                return oFrameStageNotify(m_iStage)
            end

            pcall(function ()
                for _, func in pairs(FrameStage) do
                    func(m_iStage)
                end
            end)
            return oFrameStageNotify(m_iStage)
        end

        oFrameStageNotify = Hooks.Create(Signatures.VClient018).Hook("void(__stdcall*)(int)", hkFrameStageNotify, 37)

        Utils.UnloadFunction(function ()
            oFrameStageNotify.unHookAll()
        end)
    end)()
    fsn = nil -- release memory

    return {
        CreateMove = function(func)
            CreateMove[#CreateMove+1] = func
        end,
        Draw = function (func)
            Draw[#Draw+1] = func
        end,
        FrameStage = function(func)
            FrameStage[#FrameStage+1] = func
        end,
        AnimationUpdate = function (func)
            Animations[#Animations+1] = func
        end
    }
end)()

local Render, EFontFlags, RoundingFlags = (function ()
    local ScreenSize = Engine.GetScreenSize()

    ffi.cdef[[
        typedef struct {
            vec2_t m_Position;
            vec2_t m_TexCoord;
        } Vertex_t;
    ]]

    local native_Surface, Cast_m_bClippingEnabled, clipCache, PrintText, font_cache, ConvertAnsiToUnicode, VGUI_Surface031 = (function ()
        local Localize = ffi.cast(ffi.typeof('void***'), Utils.CreateInterface('localize.dll', 'Localize_001'))

        local ConvertAnsiToUnicode = Utils.VTableBind(Localize, 15, 'int(__thiscall*)(void*, const char*, wchar_t*, int)')
        local ConvertUnicodeToAnsi = Utils.VTableBind(Localize, 16, 'int(__thiscall*)(void*, wchar_t*, char*, int)')

        Localize = nil -- release memory

        local VGUI_Surface031 = Utils.CreateInterface("vguimatsurface.dll", "VGUI_Surface031")
        local g_VGuiSurface = ffi.cast(ffi.typeof("void***"), VGUI_Surface031)
        local native_Surface = {}
        native_Surface.ISurface = g_VGuiSurface
        native_Surface.UnlockCursor = Utils.VTableBind(g_VGuiSurface, "void(__thiscall*)(void)", 66) --vmt_bind(g_VGuiSurface, "void(__thiscall*)(void)", 66)
        native_Surface.LockCursor = Utils.VTableBind(g_VGuiSurface, "void(__thiscall*)(void)", 67)
        native_Surface.DrawSetColor = Utils.VTableBind(g_VGuiSurface, "void(__thiscall*)(void*, color_struct_t)", 14)
        native_Surface.DrawTexturedRect = Utils.VTableBind(g_VGuiSurface, "void(__thiscall*)(void*, int, int, int, int)", 41)
        native_Surface.DrawSetTextureRGBA = Utils.VTableBind(g_VGuiSurface, "void(__thiscall*)(void*, int, const unsigned char*, int, int)", 37)
        native_Surface.DrawSetTexture = Utils.VTableBind(g_VGuiSurface, "void(__thiscall*)(void*, int)", 38)
        native_Surface.DrawFilledRectFade = Utils.VTableBind(g_VGuiSurface, "void(__thiscall*)(void*, int, int, int, int, unsigned int, unsigned int, bool)", 123)
        native_Surface.DrawFilledRect = Utils.VTableBind(g_VGuiSurface, 'void(__thiscall*)(void*, int, int, int, int)', 16)
        native_Surface.DrawOutlinedRect = Utils.VTableBind(g_VGuiSurface, 'void(__thiscall*)(void*, int, int, int, int)', 18)
        native_Surface.CreateNewTextureID = Utils.VTableBind(g_VGuiSurface, "int(__thiscall*)(void*, bool)", 43)
        native_Surface.IsTextureIDValid = Utils.VTableBind(g_VGuiSurface, "bool(__thiscall*)(void*, int)", 42)
        native_Surface.FontCreate = Utils.VTableBind(g_VGuiSurface, 'unsigned long(__thiscall*)(void*)', 71)
        native_Surface.SetFontGlyphSet = Utils.VTableBind(g_VGuiSurface, 'void(__thiscall*)(void*, unsigned long, const char*, int, int, int, int, unsigned long, int, int)', 72)
        native_Surface.GetTextSize = Utils.VTableBind(g_VGuiSurface, 'void(__thiscall*)(void*, unsigned long, const wchar_t*, int&, int&)', 79)
        native_Surface.DrawSetTextColor = Utils.VTableBind(g_VGuiSurface, 'void(__thiscall*)(void*, int, int, int, int)', 25)
        native_Surface.DrawSetTextFont = Utils.VTableBind(g_VGuiSurface, 'void(__thiscall*)(void*, unsigned long)', 23)
        native_Surface.DrawSetTextPos = Utils.VTableBind(g_VGuiSurface, 'void(__thiscall*)(void*, int, int)', 26)
        native_Surface.DrawPrintText = Utils.VTableBind(g_VGuiSurface, 'void(__thiscall*)(void*, const wchar_t*, int, int)', 28)
        native_Surface.DrawLine = Utils.VTableBind(g_VGuiSurface, 'void(__thiscall*)(void*, int, int, int, int)', 19)
        native_Surface.DrawTexturedPolygon = Utils.VTableBind(g_VGuiSurface, "void(__thiscall*)(void*, int, Vertex_t*, bool)", 106)
        native_Surface.DrawTexturedPolyLine = Utils.VTableBind(g_VGuiSurface, "void(__thiscall*)(void*, const Vertex_t*, int)", 104)
        native_Surface.LimitDrawingArea = Utils.VTableBind(g_VGuiSurface, "void(__thiscall*)(void*, int, int, int, int)", 147)
        native_Surface.GetDrawingArea = Utils.VTableBind(g_VGuiSurface, "void(__thiscall*)(void*, int&, int&, int&, int&)", 146)

        local PrintText = function(text, localized)
            local size = 1024.0
            if localized then
                local char_buffer = ffi.typeof("char[?]")(size)
                ConvertUnicodeToAnsi(text, char_buffer, size)

                return native_Surface.DrawPrintText(text, #ffi.string(char_buffer), 0)
            else
                local wide_buffer = ffi.typeof("wchar_t[?]")(size)

                ConvertAnsiToUnicode(text, wide_buffer, size)
                return native_Surface.DrawPrintText(wide_buffer, #text, 0)
            end
        end

        local font_cache = {}

        local Cast_m_bClippingEnabled = ffi.cast('int*', VGUI_Surface031 + 0x280)

        local clipCache = {
            x = ffi.typeof("int[1]")(),
            y = ffi.typeof("int[1]")(),
            x1 = ffi.typeof("int[1]")(),
            y1 = ffi.typeof("int[1]")()
        }

        g_VGuiSurface = nil -- release memory

        return native_Surface, Cast_m_bClippingEnabled, clipCache, PrintText, font_cache, ConvertAnsiToUnicode, VGUI_Surface031
    end)()

    local font_t = {
        data = nil,
        name = '',
        height = 0,
        weight = 0,
        flags = 0x0
    }

    function font_t:new(o)
        o = o or {}
        setmetatable(o, self)
        self.__index = self
        return o
    end
    
    EFontFlags = ffi.typeof([[
        enum {
    	    NONE,
    	    ITALIC			= 0x001,
    	    UNDERLINE		= 0x002,
    	    STRIKEOUT		= 0x004,
    	    SYMBOL			= 0x008,
    	    ANTIALIAS		= 0x010,
    	    GAUSSIANBLUR		= 0x020,
    	    ROTARY			= 0x040,
    	    DROPSHADOW		= 0x080,
    	    ADDITIVE		= 0x100,
    	    OUTLINE		= 0x200,
    	    CUSTOM			= 0x400,
        }
    ]])

    local Font = function(name, h, weight, flags, blur)
        local flags_i = 0
        local t = type(flags)
        if t == "number" then
            flags_i = flags
        elseif t == "table" then
            for i = 1, #flags do
                flags_i = flags_i + flags[i]
            end
        else
            flags_i = 0x0
        end

        t = nil -- release memory
    
        local cache_key = string.format("%s\0%d\0%d\0%d", name, h, weight, flags_i)
        if font_cache[cache_key] == nil then
            font_cache[cache_key] = font_t:new()
            font_cache[cache_key].data = native_Surface.FontCreate()
            font_cache[cache_key].name = name
            font_cache[cache_key].height = h
            font_cache[cache_key].weight = weight
            font_cache[cache_key].flags = flags_i
            native_Surface.SetFontGlyphSet(font_cache[cache_key].data, name, h, weight or 500, blur or 0, 0, flags_i, 0, 0)
        end
        flags_i = nil -- release memory
        
        return font_cache[cache_key]
    end

    local MassCreateFont = function(name, h_min, h_max, weight_min, weight_max, flags)
        local ret = {}
        for i = h_min, h_max do
            ret[i] = {}
            for j = weight_min, weight_max, 100 do
                ret[i][j] = Font(name, i, j, flags)
            end
        end
        return ret
    end

    local Text = function(font, pos, clr, text)
        native_Surface.DrawSetTextPos(pos.x, pos.y)
        native_Surface.DrawSetTextFont(font.data)
        native_Surface.DrawSetTextColor(clr:Expand())
        return PrintText(text, false)
    end

    function font_t:Measure(text)
        if not text or text == '' or type(text) ~= 'string' then
            return vec2_t()
        end
        local wide_buffer = ffi.typeof("wchar_t[?]")(1024)
        local w_ptr = ffi.typeof("int[1]")()
        local h_ptr = ffi.typeof("int[1]")()
    
        ConvertAnsiToUnicode(text, wide_buffer, 1024)
        native_Surface.GetTextSize(self.data, wide_buffer, w_ptr, h_ptr)
    
        return vec2_t(tonumber(w_ptr[0]), tonumber(h_ptr[0]))
    end

    function font_t:Text(pos, clr, text, ...)
        if not pos or not pos.x or not pos.y then return Utils.SafeError("font_t:Text", "Invalid Position") end
        if not clr or not clr.r or not clr.g or not clr.b then return Utils.SafeError("font_t:Text", "Invalid Color") end
        if not text or type(text) == "table" then return Utils.SafeError("font_t:Text", "Invalid Text") end
        if type(text) ~= "string" then text = tostring(text) end

        local a = {...}
        if #a > 0 then
            local draw_clr = clr
            local offset = self:Measure(text).x
            Text(self, pos, clr, text)
            for i = 1, #a do
                if type(a[i]) == 'string' then
                    Text(self, vec2_t(pos.x + offset, pos.y), draw_clr, a[i])
                    offset = offset + self:Measure(a[i]).x
                else
                    if a[i].a then
                        draw_clr = a[i]
                    end
                end
            end
            draw_clr = nil -- release memory
            offset = nil -- release memory
            return
        end
        return Text(self, pos, clr, text)
    end

    local vec2_t_zero = ffi.new(ffi.typeof('vec2_t'), {x = 0, y = 0})

    local Polygon = function(vertices, clipvertices, clr)
        local numvert = #vertices
        local buf = ffi.typeof('Vertex_t[?]')(numvert)
        local i = 0
        for k, vec in pairs(vertices) do
            buf[i].m_Position = vec
            buf[i].m_TexCoord = vec2_t_zero
            i = i + 1
        end
        native_Surface.DrawSetColor(ffi.new('color_struct_t', {clr.r, clr.g, clr.b, clr.a}))
        native_Surface.DrawSetTexture(-1)
        native_Surface.DrawTexturedPolygon(numvert, buf, clipvertices)
        i = nil
    end

    local Polyline = function(vertices, clr)
        local numvert = #vertices
        local buf = ffi.typeof('Vertex_t[?]')(numvert)
        local i = 0
        for k, vec in pairs(vertices) do
            buf[i].m_Position = vec
            buf[i].m_TexCoord = vec2_t_zero
            i = i + 1
        end
        native_Surface.DrawSetColor(clr)
        native_Surface.DrawSetTexture(-1)
        native_Surface.DrawTexturedPolyLine(buf, numvert)
    end
    vec2_t_zero = nil -- release memory

    local SetClip = function(vec, size)
        Cast_m_bClippingEnabled[0] = true
        native_Surface.GetDrawingArea(clipCache.x, clipCache.y, clipCache.x1, clipCache.y1)
        native_Surface.LimitDrawingArea(vec.x, vec.y, vec.x + size.x, vec.y + size.y)
    end
    
    local EndClip = function()
        native_Surface.LimitDrawingArea(clipCache.x[0], clipCache.y[0], clipCache.x1[0], clipCache.y1[0])
        Cast_m_bClippingEnabled[0] = false
    end
    
    local Line = function(pos1, pos2, clr)
        native_Surface.DrawSetColor(clr)
        return native_Surface.DrawLine(pos1.x, pos1.y, pos2.x, pos2.y)
    end

    local draw_func_gradient_rect = function(pos, dim, clr, clr2, horizontal)
        horizontal = horizontal or false
        native_Surface.DrawSetColor(ffi.new('color_struct_t', {clr.r, clr.g, clr.b, clr.a}))
        native_Surface.DrawFilledRectFade(pos.x, pos.y, pos.x + dim.x, pos.y + dim.y, 255, 0, horizontal)
        native_Surface.DrawSetColor(ffi.new('color_struct_t', {clr2.r, clr2.g, clr2.b, clr2.a}))
        return native_Surface.DrawFilledRectFade(pos.x, pos.y, pos.x + dim.x, pos.y + dim.y, 0, 255, horizontal)
    end

    local draw_filled_rect_box = function(pos, dim, clr)
        native_Surface.DrawSetColor(clr)
        return native_Surface.DrawFilledRect(pos.x, pos.y, pos.x + dim.x, pos.y + dim.y)
    end
    
    local draw_outlined_rect = function(pos, dim, clr)
        native_Surface.DrawSetColor(clr)
        return native_Surface.DrawOutlinedRect(pos.x, pos.y, pos.x + dim.x, pos.y + dim.y)
    end

    local RoundingFlags = {}
    RoundingFlags.CORNER_NONE = 0
    RoundingFlags.CORNER_TOP_LEFT = bit.lshift(1, 0)
    RoundingFlags.CORNER_TOP_RIGHT = bit.lshift(1, 1)
    RoundingFlags.CORNER_BOTTOM_LEFT = bit.lshift(1, 2)
    RoundingFlags.CORNER_BOTTOM_RIGHT = bit.lshift(1, 3)
    RoundingFlags.CORNER_TOP = bit.bor(RoundingFlags.CORNER_TOP_LEFT, RoundingFlags.CORNER_TOP_RIGHT)
    RoundingFlags.CORNER_RIGHT = bit.bor(RoundingFlags.CORNER_TOP_RIGHT, RoundingFlags.CORNER_BOTTOM_RIGHT)
    RoundingFlags.CORNER_BOTTOM = bit.bor(RoundingFlags.CORNER_BOTTOM_LEFT, RoundingFlags.CORNER_BOTTOM_RIGHT)
    RoundingFlags.CORNER_LEFT = bit.bor(RoundingFlags.CORNER_TOP_LEFT, RoundingFlags.CORNER_BOTTOM_LEFT)
    RoundingFlags.CORNER_TOP_LEFT_BOTTOM_RIGHT = bit.bor(RoundingFlags.CORNER_TOP_LEFT, RoundingFlags.CORNER_BOTTOM_RIGHT)
    RoundingFlags.CORNER_TOP_RIGHT_BOTTOM_LEFT = bit.bor(RoundingFlags.CORNER_TOP_RIGHT, RoundingFlags.CORNER_BOTTOM_LEFT)
    RoundingFlags.CORNER_ALL = bit.bor(RoundingFlags.CORNER_TOP, RoundingFlags.CORNER_RIGHT, RoundingFlags.CORNER_BOTTOM, RoundingFlags.CORNER_LEFT)

    function draw_rect_rounded(pos, dim, clr, rounding, flags, is_filled)
        if not flags and rounding and rounding > 0.5 then
            flags = RoundingFlags.CORNER_ALL
        elseif not flags and not rounding then
            flags = RoundingFlags.CORNER_NONE
        end
        local f_rectangle_vertices = {}
        rounding = rounding or 0
        if rounding > 0.5 and flags ~= RoundingFlags.CORNER_NONE then
            local round_top_left = bit.band(flags, RoundingFlags.CORNER_TOP_LEFT) ~= 0
            local round_top_right = bit.band(flags, RoundingFlags.CORNER_TOP_RIGHT) ~= 0
            local round_bottom_left = bit.band(flags, RoundingFlags.CORNER_BOTTOM_LEFT) ~= 0
            local round_bottom_right = bit.band(flags, RoundingFlags.CORNER_BOTTOM_RIGHT) ~= 0

            local num_segments = rounding * 4
        
            for i = 0, 3 do
                local round_corner = true

                if i == 0 then
                    round_corner = round_top_right
                elseif i == 1 then
                    round_corner = round_bottom_right
                elseif i == 2 then
                    round_corner = round_bottom_left
                elseif i == 3 then
                    round_corner = round_top_left
                end

                local corner_point = vec2_t(
                    pos.x + ((i < 2) and (dim.x - (round_corner and rounding or 0)) or round_corner and rounding or 0),
                    pos.y + ((i % 3 ~= 0) and (dim.y - (round_corner and rounding or 0)) or round_corner and rounding or 0)
                )

                if round_corner then
                    for p = 0, num_segments - 1 do
                        local angle = math.deg2rad(90 * (i - 1) + (90 / num_segments) * p)

                        f_rectangle_vertices[#f_rectangle_vertices + 1] = vec2_t(corner_point.x + rounding * math.cos(angle), corner_point.y + rounding * math.sin(angle))
                    end
                else
                    f_rectangle_vertices[#f_rectangle_vertices + 1] = corner_point
                end
            end
            f_rectangle_vertices[#f_rectangle_vertices + 1] = f_rectangle_vertices[1]

            if is_filled then
                Polygon(f_rectangle_vertices, true, clr)
            else
                Polyline(f_rectangle_vertices, clr)
            end
        else
            if is_filled then
                draw_filled_rect_box(pos, dim, clr)
            else
                draw_outlined_rect(pos, dim, clr)
            end
        end
    end

    function draw_filled_rect_rounded(pos, dim, clr, rounding, flags)
        return draw_rect_rounded(pos, dim, clr, rounding, flags, true)
    end

    function draw_outlined_rect_rounded(pos, dim, clr, rounding, flags)
        return draw_rect_rounded(pos, dim, clr, rounding, flags, false)
    end

    local FilledRect = function(pos, dim, clr, clr2, horizontal, rounding, flags)
        clr = clr or Color()
        clr2 = clr2 or nil
        horizontal = horizontal or false
        rounding = rounding or 0
        flags = flags or (rounding > 0.5 and RoundingFlags.CORNER_ALL or RoundingFlags.CORNER_NONE)
    
        if rounding > 0.5 and clr2 ~= nil and (horizontal and (rounding * 4 < dim.x) or (rounding * 4 < dim.y)) then
            if horizontal then
                SetClip(pos, vec2_t(rounding, dim.y))
                draw_filled_rect_rounded(pos, vec2_t(rounding * 2, dim.y), clr, rounding, flags)
                EndClip()
    
                SetClip(vec2_t(pos.x + dim.x - rounding, pos.y), vec2_t(rounding, dim.y))
                draw_filled_rect_rounded(vec2_t(pos.x + dim.x - rounding * 2, pos.y), vec2_t(rounding * 2, dim.y), clr2, rounding, flags)
                EndClip()
    
                draw_func_gradient_rect(vec2_t(pos.x + rounding, pos.y), vec2_t(dim.x - rounding * 2, dim.y), clr, clr2, horizontal)
            else
                SetClip(pos, vec2_t(dim.x, rounding))
                draw_filled_rect_rounded(pos, vec2_t(dim.x, rounding * 2), clr, rounding, flags)
                EndClip()
    
                SetClip(vec2_t(pos.x, pos.y + dim.y - rounding), vec2_t(dim.x, rounding * 2))
                draw_filled_rect_rounded(vec2_t(pos.x, pos.y + dim.y - rounding * 2), vec2_t(dim.x, rounding * 2), clr2, rounding, flags)
                EndClip()
                
                draw_func_gradient_rect(vec2_t(pos.x, pos.y + rounding), vec2_t(dim.x, dim.y - rounding * 2), clr, clr2, horizontal)
            end
        elseif clr2 ~= nil then
            draw_func_gradient_rect(pos, dim, clr, clr2, horizontal)
        else
            draw_filled_rect_rounded(pos, dim, clr, rounding, flags)
        end
    end
    
    local Rect = function(pos, dim, clr, clr2, horizontal, rounding, flags)
        clr = clr or Color()
        clr2 = clr2 or nil
        horizontal = horizontal or false
        rounding = rounding or 0
        flags = flags or (rounding > 0.5 and RoundingFlags.CORNER_ALL or RoundingFlags.CORNER_NONE)
    
        if rounding > 0.5 and clr2 ~= nil and (horizontal and (rounding * 4 < dim.x) or (rounding * 4 < dim.y)) then
            if horizontal then
                SetClip(pos, vec2_t(rounding, dim.y))
                draw_outlined_rect_rounded(pos, vec2_t(rounding * 2, dim.y), clr, rounding, flags)
                EndClip()
    
                SetClip(vec2_t(pos.x + dim.x - rounding, pos.y), vec2_t(rounding + 1, dim.y))
                draw_outlined_rect_rounded(vec2_t(pos.x + dim.x - rounding * 2, pos.y), vec2_t(rounding * 2, dim.y), clr2, rounding, flags)
                EndClip()
    
                draw_func_gradient_rect(vec2_t(pos.x + rounding, pos.y), vec2_t(dim.x - rounding * 2, 1), clr, clr2, horizontal)
                draw_func_gradient_rect(vec2_t(pos.x + rounding/2, pos.y + dim.y - 1), vec2_t(dim.x - rounding, 1), clr, clr2, horizontal)
            else
                SetClip(pos, vec2_t(dim.x, rounding))
                draw_outlined_rect_rounded(pos, vec2_t(dim.x, rounding * 2), clr, rounding, flags)
                EndClip()
    
                SetClip(vec2_t(pos.x, pos.y + dim.y - rounding), vec2_t(dim.x, rounding * 2))
                draw_outlined_rect_rounded(vec2_t(pos.x, pos.y + dim.y - rounding * 2), vec2_t(dim.x, rounding * 2), clr2, rounding, flags)
                EndClip()
    
                draw_func_gradient_rect(vec2_t(pos.x, pos.y + rounding), vec2_t(1, dim.y - rounding * 2), clr, clr2, horizontal)
                draw_func_gradient_rect(vec2_t(pos.x + dim.x - 1, pos.y + rounding/2), vec2_t(1, dim.y - rounding), clr, clr2, horizontal)
            end
        elseif clr2 ~= nil then
            if horizontal then
                draw_func_gradient_rect(pos, vec2_t(dim.x, 1), clr, clr2, horizontal)
                draw_func_gradient_rect(vec2_t(pos.x, pos.y + dim.y - 1), vec2_t(dim.x, 1), clr, clr2, horizontal)
                draw_outlined_rect(pos, vec2_t(1, dim.y), clr)
                draw_outlined_rect(vec2_t(pos.x + dim.x - 1, pos.y), vec2_t(1, dim.y), clr2)
            else
                draw_func_gradient_rect(pos, vec2_t(1, dim.y), clr, clr2, horizontal)
                draw_func_gradient_rect(vec2_t(pos.x + dim.x - 1, pos.y), vec2_t(1, dim.y), clr, clr2, horizontal)
                draw_outlined_rect(pos, vec2_t(dim.x, 1), clr)
                draw_outlined_rect(vec2_t(pos.x, pos.y + dim.y - 1), vec2_t(dim.x, 1), clr2)
            end
        else
            draw_outlined_rect_rounded(pos, dim, clr, rounding, flags)
        end
    end

    function circle_vertices(pos, radius, start_angle, end_angle)
        local start_angle = start_angle or 0
        local end_angle = end_angle or 360
        local vertices = {}
    
        local step = 15
        step = end_angle >= start_angle and step or -step
    
        for i = start_angle, end_angle, step do
            local i_rad = math.rad(i)
            local point = vec2_t(pos.x + math.cos(i_rad) * radius, pos.y + math.sin(i_rad) * radius)
            vertices[#vertices + 1] = point
        end
    
        for i = #vertices, 1, -1 do
            vertices[#vertices + 1] = vertices[i]
        end
    
        return vertices
    end
    
    local Circle = function(pos, radius, color, start_angle, end_angle)
        Polyline(circle_vertices(pos, radius, start_angle, end_angle), color)
    end
    
    local FilledCircle = function(pos, radius, color, start_angle, end_angle)
        Polygon(circle_vertices(pos, radius, start_angle, end_angle), true, color)
    end
    
    local Circle3D = function(Position, flRadius, Color)
        local nResolution = math.floor((flRadius * 2.0) + 2)
        local vertices = {}
    
        for i = 0, nResolution, 1 do
            local rotation = (math.pi * (i / (nResolution / 2.0)))
            local wts = vec2_t(Position.x + flRadius * math.cos(rotation), Position.y + flRadius * math.sin(rotation), Position.z)
    
            if wts ~= nil then
                if wts.x - flRadius/2 < ScreenSize.x and wts.x + flRadius/2 > 0 and wts.y - flRadius/2 < ScreenSize.y and wts.y + flRadius/2 > 0 then
                    table.insert(vertices, vec2_t(wts.x, wts.y))
                end
            end
        end
    
        Polygon(vertices, true, Color)
    end

    local InitTexture = function(data, w, h)
        local returntbl = {}
        local tid = native_Surface.CreateNewTextureID(true)
        if not tid or not native_Surface.IsTextureIDValid(tid) or not data then
            return
        end
        native_Surface.DrawSetTextureRGBA(tid, data, w, h)
        returntbl.tid = tid
        returntbl.data = data
        return returntbl
    end
    
    local Image = function(datatbl, pos, dim, alpha)
        local tid = datatbl.tid
        native_Surface.DrawSetColor(ffi.new('color_struct_t', {255, 255, 255, alpha}))
        native_Surface.DrawSetTexture(tid)
        return native_Surface.DrawTexturedRect(pos.x, pos.y, pos.x + dim.x, pos.y + dim.y)
    end

    return {
        Font = Font,
        MassCreateFont = MassCreateFont,
        Polygon = Polygon,
        Polyline = Polyline,
        Line = Line,
        FilledRect = FilledRect,
        Rect = Rect,
        FilledCircle = FilledCircle,
        Circle = Circle,
        Circle3D = Circle3D,
        InitTexture = InitTexture,
        Image = Image
    }, EFontFlags, RoundingFlags
end)()

return {
    vec2_t = vec2_t,
    vec3_t = vec3_t,
    HSV = HSV,
    HEX = HEX,
    Color = Color,
    Utils = Utils,
    Clipboard = Clipboard,
    Hooks = Hooks,
    ClientState = ClientState,
    NetGraph = NetGraph,
    Engine = Engine,
    NetChannel = NetChannel,
    --NetChannelInfo = NetChannelInfo, -- Crashes ATM
    Entity = Entity,
    Input = Input,
    Callbacks = Callbacks,
    Render = Render,
    EFontFlags = EFontFlags,
    RoundingFlags = RoundingFlags,
    User = User
}
