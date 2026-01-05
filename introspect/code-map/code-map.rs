// Run from anywhere:
//   cargo run -p code-map
//
// Generates introspect/code-map/code-map.org with a hierarchical view of the rust/ codebase.

use std::fs;
use std::path::Path;
use std::env;
use syn::{Item, ImplItem, File};
use walkdir::WalkDir;

fn main() {
    // Find project root (where Cargo.toml with [workspace] lives)
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let project_root = Path::new(manifest_dir).parent().unwrap().parent().unwrap();

    let rust_dir = project_root.join("rust");
    let output_file = project_root.join("introspect/code-map/code-map.org");

    // Paths in org links are relative to where the org file lives
    let output = generate_code_map(&rust_dir, "../../rust");

    fs::write(&output_file, &output).expect("Failed to write output");
    println!("Generated {} ({} lines)", output_file.display(), output.lines().count());
}

fn generate_code_map(root: &Path, org_path_prefix: &str) -> String {
    let mut output = String::new();
    output.push_str("* rust/\n");
    process_directory(root, &mut output, 2, org_path_prefix);
    output
}

fn process_directory(dir: &Path, output: &mut String, level: usize, org_path: &str) {
    let mut entries: Vec<_> = WalkDir::new(dir)
        .min_depth(1)
        .max_depth(1)
        .into_iter()
        .filter_map(|e| e.ok())
        .collect();

    // Sort alphabetically (so typedb.rs appears next to typedb/)
    entries.sort_by(|a, b| a.file_name().cmp(b.file_name()));

    let stars = "*".repeat(level);

    for entry in entries {
        let path = entry.path();
        let name = path.file_name().unwrap().to_string_lossy();

        if path.is_dir() {
            let new_org_path = format!("{}/{}", org_path, name);
            output.push_str(&format!("{} {}/\n", stars, name));
            process_directory(path, output, level + 1, &new_org_path);
        } else if name.ends_with(".rs") {
            let file_org_path = format!("{}/{}", org_path, name);
            output.push_str(&format!("{} {}\n", stars, name));
            process_rust_file(path, output, level + 1, &file_org_path);
        }
    }
}

fn process_rust_file(path: &Path, output: &mut String, level: usize, org_path: &str) {
    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(_) => return,
    };

    let syntax: File = match syn::parse_file(&content) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("Failed to parse {:?}: {}", path, e);
            return;
        }
    };

    let stars = "*".repeat(level);
    let child_stars = "*".repeat(level + 1);

    for item in &syntax.items {
        match item {
            Item::Fn(f) => {
                let name = &f.sig.ident;
                let line = line_of_span(&content, f.sig.fn_token.span);
                output.push_str(&format!("{} [[file:{}::{}][fn {}]]\n", stars, org_path, line, name));
            }
            Item::Struct(s) => {
                let name = &s.ident;
                let line = line_of_span(&content, s.struct_token.span);
                output.push_str(&format!("{} [[file:{}::{}][struct {}]]\n", stars, org_path, line, name));
            }
            Item::Enum(e) => {
                let name = &e.ident;
                let line = line_of_span(&content, e.enum_token.span);
                output.push_str(&format!("{} [[file:{}::{}][enum {}]]\n", stars, org_path, line, name));
            }
            Item::Type(t) => {
                let name = &t.ident;
                let line = line_of_span(&content, t.type_token.span);
                output.push_str(&format!("{} [[file:{}::{}][type {}]]\n", stars, org_path, line, name));
            }
            Item::Trait(t) => {
                let name = &t.ident;
                let line = line_of_span(&content, t.trait_token.span);
                output.push_str(&format!("{} [[file:{}::{}][trait {}]]\n", stars, org_path, line, name));
            }
            Item::Const(c) => {
                let name = &c.ident;
                let line = line_of_span(&content, c.const_token.span);
                output.push_str(&format!("{} [[file:{}::{}][const {}]]\n", stars, org_path, line, name));
            }
            Item::Static(s) => {
                let name = &s.ident;
                let line = line_of_span(&content, s.static_token.span);
                output.push_str(&format!("{} [[file:{}::{}][static {}]]\n", stars, org_path, line, name));
            }
            Item::Macro(m) => {
                if let Some(ident) = &m.ident {
                    let line = line_of_span(&content, m.mac.path.segments[0].ident.span());
                    output.push_str(&format!("{} [[file:{}::{}][macro {}]]\n", stars, org_path, line, ident));
                }
            }
            Item::Impl(imp) => {
                let impl_name = format_impl_name(imp);
                let line = line_of_span(&content, imp.impl_token.span);
                output.push_str(&format!("{} [[file:{}::{}][impl {}]]\n", stars, org_path, line, impl_name));

                // Process impl items
                for impl_item in &imp.items {
                    match impl_item {
                        ImplItem::Fn(f) => {
                            let name = &f.sig.ident;
                            let iline = line_of_span(&content, f.sig.fn_token.span);
                            output.push_str(&format!("{} [[file:{}::{}][fn {}]]\n", child_stars, org_path, iline, name));
                        }
                        ImplItem::Type(t) => {
                            let name = &t.ident;
                            let iline = line_of_span(&content, t.type_token.span);
                            output.push_str(&format!("{} [[file:{}::{}][type {}]]\n", child_stars, org_path, iline, name));
                        }
                        ImplItem::Const(c) => {
                            let name = &c.ident;
                            let iline = line_of_span(&content, c.const_token.span);
                            output.push_str(&format!("{} [[file:{}::{}][const {}]]\n", child_stars, org_path, iline, name));
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }
}

fn line_of_span(_content: &str, span: proc_macro2::Span) -> usize {
    span.start().line
}

fn format_impl_name(imp: &syn::ItemImpl) -> String {
    let self_ty = quote_type(&imp.self_ty);

    if let Some((_, trait_path, _)) = &imp.trait_ {
        let trait_name = trait_path
            .segments
            .iter()
            .map(|s| {
                let ident = s.ident.to_string();
                match &s.arguments {
                    syn::PathArguments::None => ident,
                    syn::PathArguments::AngleBracketed(args) => {
                        let inner: Vec<String> = args.args.iter().map(|arg| {
                            match arg {
                                syn::GenericArgument::Type(t) => quote_type(t),
                                syn::GenericArgument::Lifetime(lt) => format!("'{}", lt.ident),
                                _ => "_".to_string(),
                            }
                        }).collect();
                        format!("{}<{}>", ident, inner.join(", "))
                    }
                    syn::PathArguments::Parenthesized(_) => format!("{}(...)", ident),
                }
            })
            .collect::<Vec<_>>()
            .join("::");
        format!("{} for {}", trait_name, self_ty)
    } else {
        self_ty
    }
}

fn quote_type(ty: &syn::Type) -> String {
    match ty {
        syn::Type::Path(tp) => {
            tp.path.segments
                .iter()
                .map(|s| {
                    let ident = s.ident.to_string();
                    match &s.arguments {
                        syn::PathArguments::None => ident,
                        syn::PathArguments::AngleBracketed(args) => {
                            let inner: Vec<String> = args.args.iter().map(|arg| {
                                match arg {
                                    syn::GenericArgument::Type(t) => quote_type(t),
                                    syn::GenericArgument::Lifetime(lt) => format!("'{}", lt.ident),
                                    _ => "_".to_string(),
                                }
                            }).collect();
                            format!("{}<{}>", ident, inner.join(", "))
                        }
                        syn::PathArguments::Parenthesized(_) => format!("{}(...)", ident),
                    }
                })
                .collect::<Vec<_>>()
                .join("::")
        }
        syn::Type::Reference(r) => {
            let mutability = if r.mutability.is_some() { "mut " } else { "" };
            let lifetime = r.lifetime.as_ref()
                .map(|lt| format!("'{} ", lt.ident))
                .unwrap_or_default();
            format!("&{}{}{}", lifetime, mutability, quote_type(&r.elem))
        }
        syn::Type::Tuple(t) => {
            let inner: Vec<String> = t.elems.iter().map(quote_type).collect();
            format!("({})", inner.join(", "))
        }
        syn::Type::Slice(s) => format!("[{}]", quote_type(&s.elem)),
        syn::Type::Array(a) => format!("[{}; _]", quote_type(&a.elem)),
        syn::Type::Ptr(p) => {
            let mutability = if p.mutability.is_some() { "mut " } else { "const " };
            format!("*{}{}", mutability, quote_type(&p.elem))
        }
        _ => "_".to_string(),
    }
}
