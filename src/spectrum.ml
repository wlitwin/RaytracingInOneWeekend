type spectrum = float array

let whitepoints =
    Array.of_list [
		Vec.mk 1.00000000 0.18172716 0.00000000; (* 1000K *)
		Vec.mk 1.00000000 0.42322816 0.00000000;
		Vec.mk 1.00000000 0.54360078 0.08679949;
		Vec.mk 1.00000000 0.64373109 0.28819679;
		Vec.mk 1.00000000 0.71976951 0.42860152;
		Vec.mk 1.00000000 0.77987699 0.54642268;
		Vec.mk 1.00000000 0.82854786 0.64816570;
		Vec.mk 1.00000000 0.86860704 0.73688797;
		Vec.mk 1.00000000 0.90198230 0.81465502;
		Vec.mk 1.00000000 0.93853986 0.88130458;
		Vec.mk 1.00000000 0.97107439 0.94305985;
		Vec.mk 1.00000000 1.00000000 1.00000000; (* 6500K *)
		Vec.mk 0.95160805 0.96983355 1.00000000;
		Vec.mk 0.91194747 0.94470005 1.00000000;
		Vec.mk 0.87906581 0.92357340 1.00000000;
		Vec.mk 0.85139976 0.90559011 1.00000000;
		Vec.mk 0.82782969 0.89011714 1.00000000;
		Vec.mk 0.80753191 0.87667891 1.00000000;
		Vec.mk 0.78988728 0.86491137 1.00000000; (* 10000K *)
		Vec.mk 0.77442176 0.85453121 1.00000000;
    ]
;;

let color_to_spectrum (color, wavelength) =
    1.0
;;

let create_spectrum () =
    (* 380 - 780 nm in 5nm increments *)
    Array.make ((780 - 380) / 5) 0.
;;

let spectrum_to_color spectrum = 
	let temp = 6500 - 1000 in
	let ratio = float (temp mod 500) /. 500. in
	let avg g = (g whitepoints.(temp / 500)) *. (1. -. ratio)
		        +. (g whitepoints.(temp / 500 + 1)) *. ratio in
	let gammar = avg Vec.gx 
	and gammag = avg Vec.gy
	and gammab = avg Vec.gz in
	Vec.zero
;;
