with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Integer_Text_IO;    use Ada.Integer_Text_IO;
with Ada.Unchecked_Deallocation;

package body Registre is

	procedure Free is
		new Ada.Unchecked_Deallocation (Object => T_Cellule, Name => T_Registre);


	-- Trouver si une clé est déjà présente ou non, utilisée dans Creer_Cle
	-- Paramètres :
	--				Reg : in T_Registre	 Registre dans lequel on cherche
	--				Cle : in Entier		 Clé provisoire en attente d'unicité
	function Est_Present(Reg : in T_Registre; Cle : in Integer; X : in out Integer) return Integer is
	begin
		if Reg = Null then
			return X;
		elsif Reg.all.Cle < Cle then
			return Est_Present (Reg.all.Sous_Arbre_Droit, Cle, X);
		elsif Reg.all.Cle > Cle then
			return Est_Present (Reg.all.Sous_Arbre_Gauche, Cle, X);
		else -- { Reg.all.Cle = Cle }
			X := X + 1;
			-- On relance une recherche sur la clé Cle + 1
			return Est_Present(Reg, Cle+1, X);
		end if;
	end Est_Present;


    procedure Creer_Cle (Reg : in T_Registre; Cle : out Integer; Donnee : in T_Donnee) is
		X : Integer;
		Cle_Provisoire : Integer;
    begin
		-- On cherche si d'autres individus sont nés le même jour
		Cle_Provisoire := 100000000 * Donnee.Mois_N + 1000000 * Donnee.Jour_N + 100*Donnee.Annee_N;
		X := 0;

		-- Si la clé est déjà présente on incrémente X et on cherche si la clé avec X+1 est présente etc...
		X := Est_Present (Reg, Cle_Provisoire, X);

		-- Finalement on change la clé !
        Cle := Cle_Provisoire + X;
    end Creer_Cle;


    procedure Initialiser_Donnee (  Donnee : out T_Donnee;
                                    Nom : in unbounded_string;
                                    Prenom : in unbounded_string;
                                    Jour_N : in Integer;
                                    Mois_N : in Integer;
                                    Annee_N : in Integer;
                                    Sexe : in Character) is
    begin
        Donnee.Nom := Nom;
        Donnee.Prenom := Prenom;
        Donnee.Jour_N := Jour_N;
        Donnee.Mois_N := Mois_N;
        Donnee.Annee_N := Annee_N;
        Donnee.Sexe := Sexe;
    end Initialiser_Donnee;


	procedure Initialiser_Reg(Reg: out T_Registre; Cle : in Integer; Donnee : in T_Donnee) is
		Cellule : T_Registre;
	begin
		Cellule :=  new T_Cellule;
        Cellule.all.Cle := Cle;
        Cellule.all.Sous_Arbre_Droit := null;
        Cellule.all.Sous_Arbre_Gauche := null;
		Cellule.all.Donnee := Donnee;
        Reg := Cellule;
	end Initialiser_Reg;


	function Est_Vide (Reg : T_Registre) return Boolean is
	begin
		return Reg = Null;
	end;


	function Taille (Reg : in T_Registre) return Integer is
	begin
		if Reg = Null then
			return 0;
		else
			return 1 + Taille (Reg.all.Sous_Arbre_Gauche)
					 + Taille (Reg.all.Sous_Arbre_Droit);
		end if;
	end Taille;


	procedure Inserer (Reg : in out T_Registre ; Cle : in Integer ; Donnee : in T_Donnee) is
	begin
		if Reg = Null then
			Reg :=  new T_Cellule'(Cle, Donnee, Null, Null);
		elsif Reg.all.Cle < Cle then
			Inserer (Reg.all.Sous_Arbre_Droit, Cle, Donnee);
		elsif Reg.all.Cle > Cle then
			Inserer (Reg.all.Sous_Arbre_Gauche, Cle, Donnee);
		else -- { Reg.all.Cle = Cle }
			raise Cle_Presente_Exception_Reg;
		end if;
	end Inserer;


	-- Retourner la cellule qui contient la clé Cle.
	-- Exception : Cle_Absente_Exception_Reg si la clé est absente
	function La_Cellule (Reg: in T_Registre ; Cle : in Integer) return T_Registre is
	begin
		if Reg = Null then
			raise Cle_Absente_Exception_Reg;
		elsif Reg.all.Cle < Cle then
			return La_Cellule (Reg.all.Sous_Arbre_Droit, Cle);
		elsif Reg.all.Cle > Cle then
			return La_Cellule (Reg.all.Sous_Arbre_Gauche, Cle);
		else -- { Abr.all.Cle = Cle }
			return Reg;
		end if;
	end La_Cellule;


	procedure Modifier (Reg : in out T_Registre ; Cle : in Integer ; Donnee : in T_Donnee) is
	begin
		La_Cellule (Reg, Cle).all.Donnee := Donnee;
	end Modifier;


	function La_Donnee (Reg : in T_Registre ; Cle : in Integer) return T_Donnee is
	begin
		return La_Cellule (Reg, Cle).all.Donnee;
	end La_Donnee;

	function Retourner_Nom (Reg : in T_Registre ; Cle : in Integer) return unbounded_string is
	begin
		return La_Cellule (Reg, Cle).all.Donnee.Nom;
	end Retourner_Nom;


	-- Décrocher la cellule de Reg qui a la plus petite clé.  elle est récupérée
	-- dans Min.  Reg ne peut pas etre vide, puisqu'on initialise un premier individu
	-- donc pas d'exception.
	procedure Decrocher_Min (Reg : in out T_Registre; Min : out T_Registre) with
    		Post => Min /= Null
			and then Taille (Min) = 1		-- une feuille
			and then not (Reg'Old.all.Cle < Min.all.Cle)	-- plus petite clé !
			--and then Taille (Reg) = Taille (Reg)'Old - 1	-- un décroché
	is
	begin
        if Reg = null then
            null;
		elsif Reg.all.Sous_Arbre_Gauche = Null then --  min trouvé
			Min := Reg;
			Reg := Reg.all.Sous_Arbre_Droit;
		else
			Decrocher_Min (Reg.all.Sous_Arbre_Gauche, Min);
		end if;
	end Decrocher_Min;


	procedure Supprimer (Reg : in out T_Registre ; Cle : in Integer) is
		A_Detruire : T_Registre;
	begin
		if Reg = Null then
			raise Cle_Absente_Exception_Reg;
		elsif Reg.all.Cle < Cle then
			Supprimer (Reg.all.Sous_Arbre_Droit, Cle);
		elsif Reg.all.Cle > Cle then
			Supprimer (Reg.all.Sous_Arbre_Gauche, Cle);
		else -- { Reg.all.Cle = Cle }
			-- Supprimer le noeud Reg
			A_Detruire := Reg;
			if Reg.all.Sous_Arbre_Gauche = Null then
				Reg := Reg.all.Sous_Arbre_Droit;
			elsif Reg.all.Sous_Arbre_Droit = Null then
				Reg := Reg.all.Sous_Arbre_Gauche;
			else
				declare
					Min: T_Registre;
				begin
					Decrocher_Min (Reg.all.Sous_Arbre_Droit, Min);
					Min.all.Sous_Arbre_Gauche := Reg.all.Sous_Arbre_Gauche;
					Min.all.Sous_Arbre_Droit := Reg.all.Sous_Arbre_Droit;
					Reg := Min;
				end;
			end if;

			Free (A_Detruire);
		end if;
	end Supprimer;


	procedure Vider (Reg : in out T_Registre) is
	begin
		if Reg = Null then
			Null;
		else
			Vider (Reg.all.Sous_Arbre_Gauche);
			Vider (Reg.all.Sous_Arbre_Droit);
			Free (Reg);
		end if;
	end Vider;


	procedure Afficher (Reg : in T_Registre) is
	begin
		if Reg = Null then
			Null;
		else
			Afficher (Reg.all.Sous_Arbre_Gauche);
			Put (Reg.all.Cle, Width => 0);
			Put (" : ");
			Put(Reg.all.Donnee.Sexe);
			Put(" ");
			Put(To_String(Reg.all.Donnee.Nom));
			Put(" ");
			Put(To_String(Reg.Donnee.Prenom));
			Put (" ");
			Put(Reg.all.Donnee.Jour_N, Width => 0);
			Put("/");
			Put(Reg.all.Donnee.Mois_N, Width => 0);
			Put("/");
			Put(Reg.all.Donnee.Annee_N, Width => 0);
			New_Line;
			Afficher (Reg.all.Sous_Arbre_Droit);
		end if;
	end Afficher;

	


end Registre;
