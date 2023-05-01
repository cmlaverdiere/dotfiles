local lush = require "lush"
local base = require "zenbones"

local rose = lush.hsluv(6, 62, 60)
local leaf = lush.hsluv(111, 47, 61)
local wood = lush.hsluv(32, 47, 58)
local water = lush.hsluv(236, 64, 61)
local blossom = lush.hsluv(318, 32, 58)
local sky = lush.hsluv(204, 61, 64)

-- local rose = lush.hsluv(6, 62, 60).sa(20).li(16)

-- Create some specs
local specs = lush.parse(function()
	return {
		Constant { base.String, fg = wood },
	}
end)

-- Apply specs using lush tool-chain
lush.apply(lush.compile(specs))
